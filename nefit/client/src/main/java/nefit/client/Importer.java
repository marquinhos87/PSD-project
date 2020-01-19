package nefit.client;

import nefit.shared.NefitProtos;

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
        throws IOException
    {
        if (message.hasInfo())
        {
            this.getPrompt().printMessages("New Product available:");
            this.prompt.printMessages(printInfo(message.getInfo()));
        }
        else if (message.hasResult())
        {
            final var result = message.getResult();

            this.getPrompt().printNotice(
                "Your offer for \"%s\" was %s.",
                result.getMsg(),
                result.getResult() ? "accepted" : "rejected"
            );
        }

        else if (message.hasOrdack())
        {
            printOrderAck(message.getOrdack());
        }
    }

    private String printInfo(NefitProtos.InfoI info)
    {
        StringBuilder sb = new StringBuilder();
        sb.append("\tManufacturer: " + info.getNameM());
        sb.append("\n\tProduct: " + info.getNameP());
        sb.append("\n\tMin quantity: " + info.getMinimum());
        sb.append("\n\tMax quantity: " + info.getMaximum());
        sb.append("\n\tMin unit price: " + info.getValue());
        sb.append("\n\tTime Available: " + info.getPeriod() + " seconds");
        return sb.toString();
    }

    private void printOrderAck(NefitProtos.OrderAckI ack)
    {
        if (ack.getOutdated())
        {
            this.prompt.printMessages(ack.getMsg());
        }
        if (ack.getAck())
        {
            this.prompt.printMessages("Order accepted");
        }
        else
        {
            this.prompt
                .printMessages("Order decline, because: " + ack.getMsg());
        }
    }

    private String printCommands()
    {
        StringBuilder sb = new StringBuilder();
        sb.append("You can use some commands like 'sub' 'order'\n");
        sb.append(
            "Command sub: <sub> <Name Manufacturer> [<Name Manufacturer>] ...\n");
        sb.append(
            "Command order: <order> <Name Manufacturer> <Name Product> <Quantity> <Unit Price>");
        return sb.toString();
    }
}
