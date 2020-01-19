package nefit.client;

import nefit.shared.NefitProto;
import nefit.shared.Util;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

public class Importer extends Client< NefitProto.ServerToImporter >
{
    public Importer(Prompt prompt, Connection connection, String username)
    {
        super(
            prompt,
            connection,
            username,
            NefitProto.ServerToImporter.parser(),
            new Command(
                "subscribe",
                "Manufacturer names"
            ),
            new Command(
                "offer",
                "Manufacturer name",
                "Product name",
                "Quantity",
                "Unit price"
            )
        );
    }

    @Override
    protected void handleCommand(String command, List< String > arguments)
        throws IOException
    {
        switch (command)
        {
            case "subscribe":
                this.handleCommandSubscribe(arguments);
                break;

            case "offer":
                this.handleCommandOffer(arguments);
                break;
        }
    }

    private void handleCommandSubscribe(List< String > arguments)
        throws IOException
    {
        // validate and parse arguments (we know command is "announce")

        final var manufacturers =
            arguments.get(0).isBlank()
                ? new String[] {}
                : arguments.get(0).trim().split("\\s+");

        // send subscription request to server

        final var messageSubscribe =
            NefitProto.ImporterToServerSubscribe
                .newBuilder()
                .setImporterName(this.getUsername())
                .addAllManufacturerNames(Arrays.asList(manufacturers))
                .build();

        final var messageServer =
            NefitProto.ImporterToServer
                .newBuilder()
                .setSubscribe(messageSubscribe)
                .build();

        this.getConnection().send(messageServer);
    }

    private void handleCommandOffer(List< String > arguments) throws IOException
    {
        // validate and parse arguments (we know command is "announce")

        final var manufacturerName = arguments.get(0);
        Util.ensure(!manufacturerName.isBlank(), "Invalid manufacturer name.");

        final var productName = arguments.get(1);
        Util.ensure(!productName.isBlank(), "Invalid product name.");

        final var quantity = Integer.parseInt(arguments.get(2));
        Util.ensure(quantity > 0, "Quantitiy must be positive.");

        final var unitPrice = Float.parseFloat(arguments.get(3));
        Util.ensure(unitPrice > 0, "Unit price must be positive.");

        // send message to server

        final var messageOffer =
            NefitProto.ImporterToServerOffer
                .newBuilder()
                .setImporterName(this.getUsername())
                .setManufacturerName(manufacturerName)
                .setProductName(productName)
                .setQuantity(quantity)
                .setUnitPrice(unitPrice)
                .build();

        final var messageServer =
            NefitProto.ImporterToServer
                .newBuilder()
                .setOffer(messageOffer)
                .build();

        this.getConnection().send(messageServer);

        // TODO: must wait for acknowledgment from server
    }

    @Override
    protected void handleMessage(NefitProto.ServerToImporter message)
    {
        if(message.hasOfferSubmitted()){
            this.getPrompt().printNotice("\nYour offer has been accepted");
        }

        if(message.hasOfferInvalid()){
            this.getPrompt().printNotice("\nYour offer has rejected, because: \"%s\"",message.getOfferInvalid().getErrorMessage());
        }

        if(message.hasOfferWon()){
            final var result = message.getOfferWon();

            this.getPrompt().printNotice("\nYou won the product \"%s\" from \"%s\"."
                    + "\nYou purchase %d products with a unit price of %.2f"
                    + "\nTotal = %.2f"
                    ,result.getProductName()
                    ,result.getManufacturerName()
                    ,result.getQuantity()
                    ,result.getUnitPrice()
                    ,result.getUnitPrice() * result.getQuantity());
        }

        if(message.hasOfferLose()){
            final var result = message.getOfferLose();

            this.getPrompt().printNotice("\nYou lose the product \"%s\" from \"%s\"",
                result.getProductName(),
                result.getManufacturerName());
        }

        if(message.hasNewProduct()){
            final var info = message.getNewProduct();

            this.getPrompt().printNotice(
                "\nProduct \"%s\" now available from manufacturer \"%s\"."
                    + "\n   Min. quantity: %d"
                    + "\n   Max. quantity: %d"
                    + "\n   Min. unit price: %.2f"
                    + "\n   Available for %d seconds.",
                info.getProductName(), info.getManufacturerName(), info.getMinQuantity(),
                info.getMaxQuantity(), info.getMinUnitPrice(), info.getTimeout()
            );
        }
    }
}
