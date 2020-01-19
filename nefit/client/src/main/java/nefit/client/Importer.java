package nefit.client;

import nefit.shared.NefitProtos;
import nefit.shared.Util;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

public class Importer extends Client< NefitProtos.Importer >
{
    public Importer(Prompt prompt, Connection connection, String username)
    {
        super(
            prompt,
            connection,
            username,
            NefitProtos.Importer.parser(),
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
            NefitProtos.SubS
                .newBuilder()
                .setNameI(this.getUsername())
                .addAllSubs(Arrays.asList(manufacturers))
                .build();

        final var messageServer =
            NefitProtos.Server
                .newBuilder()
                .setM3(messageSubscribe)
                .build();

        this.getConnection().send(messageServer);

        // TODO: should maybe wait for acknowledgment from server
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
            NefitProtos.OrderS
                .newBuilder()
                .setNameI(this.getUsername())
                .setNameM(manufacturerName)
                .setNameP(productName)
                .setQuant(quantity)
                .setValue(unitPrice)
                .build();

        final var messageServer =
            NefitProtos.Server
                .newBuilder()
                .setM2(messageOffer)
                .build();

        this.getConnection().send(messageServer);

        // TODO: must wait for acknowledgment from server
    }

    @Override
    protected void handleMessage(NefitProtos.Importer message)
    {
        if (message.hasInfo())
        {
            final var info = message.getInfo();

            this.getPrompt().printNotice(
                "\nProduct \"%s\" now available from manufacturer \"%s\"."
                    + "\n   Min. quantity: %d"
                    + "\n   Max. quantity: %d"
                    + "\n   Min. unit price: %.2f"
                    + "\n   Available for %d seconds.",
                info.getNameP(), info.getNameM(), info.getMinimum(),
                info.getMaximum(), info.getValue(), info.getPeriod()
            );
        }
        else if (message.hasResult())
        {
            final var result = message.getResult();

            this.getPrompt().printNotice(
                "\nYour offer for \"%s\" was %s.",
                result.getMsg(),
                result.getResult() ? "accepted" : "rejected"
            );
        }
        else if (message.hasOrdack())
        {
            final var ack = message.getOrdack();

            final String text;

            if (ack.getOutdated())
                text = ack.getMsg();
            else if (ack.getAck())
                text = "Offer registered.";
            else
                text = "Invalid offer: " + ack.getMsg();

            this.getPrompt().printNotice("\n%s", text);
        }
    }
}
