package nefit.client;

import nefit.shared.Command;
import nefit.shared.Connection;
import nefit.shared.NefitProto;
import nefit.shared.Prompt;
import nefit.shared.Util;

import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Manufacturer extends Client< NefitProto.ServerToManufacturer >
{
    private final Set< String > activeProductNames;

    public Manufacturer(Prompt prompt, Connection connection, String username)
    {
        super(
            prompt,
            connection,
            username,
            NefitProto.ServerToManufacturer.parser(),
            new Command(
                "announce",
                "Product name",
                "Min. quantity",
                "Max. quantity",
                "Min. unit price",
                "Timeout (seconds)"
            )
        );

        this.activeProductNames = new HashSet<>();
    }

    @Override
    protected void handleCommand(String command, List< String > arguments)
        throws IOException
    {
        // validate and parse arguments (we know command is "announce")

        final var productName = arguments.get(0);

        Util.ensure(!productName.isBlank(), "Invalid product name.");

        Util.ensure(
            !this.activeProductNames.contains(productName),
            "A product with this name is currently available from you."
        );

        final var minQuantity = Integer.parseInt(arguments.get(1));
        final var maxQuantity = Integer.parseInt(arguments.get(2));

        Util.ensure(
            minQuantity > 0 && maxQuantity > 0,
            "Quantities must be positive."
        );

        Util.ensure(
            maxQuantity >= minQuantity,
            "Maximum quantity must not be lower than minimum."
        );

        final var minUnitPrice = Float.parseFloat(arguments.get(3));

        Util.ensure(minUnitPrice > 0, "Minimum unit price must be positive.");

        final var timeout = Integer.parseInt(arguments.get(4));

        Util.ensure(timeout > 0, "Timeout must be positive.");

        // send announcement request to server

        final var messageAnnounce =
            NefitProto.ManufacturerToServerAnnounce
                .newBuilder()
                .setManufacturerName(this.getUsername())
                .setProductName(productName)
                .setMinQuantity(minQuantity)
                .setMaxQuantity(maxQuantity)
                .setMinUnitPrice(minUnitPrice)
                .setTimeout(timeout)
                .build();

        final var messageServer =
            NefitProto.ManufacturerToServer
                .newBuilder()
                .setAnnounce(messageAnnounce)
                .build();

        this.getConnection().send(messageServer);
    }

    @Override
    protected void handleMessage(NefitProto.ServerToManufacturer message)
    {
        this.getPrompt().print();

        if(message.hasAnnounced()){
            this.getPrompt().printNotice("\nYour product  \"%s\"as been released",message.getAnnounced().getProductName());
        }

        if(message.hasInvalid()){
            this.getPrompt().printNotice("\nYour product as an error: \"%s\"",message.getInvalid().getErrorMessage());
        }

        if(message.hasNoOffers()){
            this.getPrompt().printNotice("\nYour product \"%s\" has no offers",message.getNoOffers().getProductName());
        }

        if(message.hasSold()){

            NefitProto.ServerToManufacturerSold sold = message.getSold();

            this.getPrompt().printNotice(
                "\nSold %d units of product \"%s\" at unit price %.2f, for a" +
                    " total price of %.2f. Announcement removed.",
                sold.getQuantity(),
                sold.getProductName(),
                sold.getUnitPrice(),
                sold.getQuantity() * sold.getUnitPrice()
            );


            this.activeProductNames.remove(sold.getProductName());
        }
    }
}
