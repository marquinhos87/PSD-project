package nefit.client;

import nefit.proto.NefitProtos;

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
                "get"
            ),
            new Command(
                "subscribe",
                "Manufacturers"
            ),
            new Command(
                "offer"
            )
        );
    }

    @Override
    protected void handleCommand(String command, List< String > arguments)
        throws IOException
    {
        switch (command)
        {
            case "get":
                this.handleCommandGet(arguments);
                break;

            case "subscribe":
                this.handleCommandSubscribe(arguments);
                break;

            case "offer":
                this.handleCommandOffer(arguments);
                break;
        }
    }

    private void handleCommandGet(List< String > arguments)
    {

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

        this.getConnection().send(messageSubscribe);

        // TODO: should maybe wait for acknowledgment from server
    }

    private void handleCommandOffer(List< String > arguments)
    {
        if (fields.length != 5)
        {
            this.prompt.printWarning("Maybe forgot something");
        }
        else
        {
            try
            {
                NefitProtos.OrderS order = this.messages.createOrderS(
                    this.name, fields[1], fields[2],
                    Integer.parseInt(fields[3]), Float.parseFloat(fields[4])
                );
                Client.writeDelimited(
                    this.os,
                    NefitProtos.Server.newBuilder().setM2(order).build()
                );
            }
            catch (NumberFormatException e)
            {
                this.prompt
                    .printError("Quantity and Price have to be a number");
            }
        }
    }

    @Override
    protected void handleMessage(NefitProtos.Importer message)
        throws IOException
    {
        while (true)
        {
            try
            {
                final var importer = Client
                    .parseDelimited(this.is, NefitProtos.Importer.parser());

                if (importer.hasInfo())
                {
                    this.prompt.printMessages("New Product available:");
                    this.prompt.printMessages(printInfo(importer.getInfo()));
                }

                if (importer.hasResult())
                    printResult(importer.getResult());

                if (importer.hasOrdack())
                    printOrderAck(importer.getOrdack());

            }
            catch (IOException e)
            {
                this.prompt.printError("Something went wrong");
            }
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

    private void printResult(NefitProtos.ResultI result)
    {
        if (result.getResult())
        {
            this.prompt.printMessages("You win the order " + result.getMsg());
        }
        else
        {
            this.prompt.printMessages("You lose the order " + result.getMsg());
        }
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
