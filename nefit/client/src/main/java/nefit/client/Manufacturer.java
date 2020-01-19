package nefit.client;

import nefit.proto.NefitProtos;

import java.io.IOException;

public class Manufacturer
    extends Client< NefitProtos.ServerToClientManufacturer >
{
    public Manufacturer(Connection connection, String username)
    {
        super(
            connection,
            username,
            NefitProtos.ServerToClientManufacturer.parser(),
            new Command[] {
                new Command(
                    "announce",
                    "Product name",
                    "Min. quantity",
                    "Max. quantity",
                    "Min. unit price",
                    "Timeout (seconds)"
                )
            }
        );
    }

    @Override
    protected void handleCommand(String command, String[] arguments)
        throws IOException
    {
        switch (command)
        {
            case "announce":
                this.handleCommandAnnounce(arguments);
                break;
        }
    }

    private void handleCommandAnnounce(String[] arguments) throws IOException
    {
        // validate and parse arguments

        final var productName = arguments[0];

        if (productName.isBlank())
            throw new IllegalArgumentException("Invalid product name.");

        final var minQuantity = Integer.parseInt(arguments[1]);
        final var maxQuantity = Integer.parseInt(arguments[2]);

        if (minQuantity <= 0 || maxQuantity <= 0)
            throw new IllegalArgumentException("Quantities must be positive.");

        if (maxQuantity < minQuantity)
        {
            throw new IllegalArgumentException(
                "Maximum quantity must not be lower than minimum."
            );
        }

        final var minUnitPrice = Float.parseFloat(arguments[3]);

        if (minUnitPrice <= 0)
        {
            throw new IllegalArgumentException(
                "Minimum unit price must be positive."
            );
        }

        final var timeout = Integer.parseInt(arguments[4]);

        if (minUnitPrice <= 0)
            throw new IllegalArgumentException("Timeout must be positive.");

        // send announcement request to server

        final var message =
            NefitProtos.DisponibilityS
                .newBuilder()
                .setNameM(this.getUsername())
                .setNameP(productName)
                .setMinimum(minQuantity)
                .setMaximum(maxQuantity)
                .setValue(minUnitPrice)
                .setPeriod(timeout)
                .build();

        this.sendMessage(message);
    }

    @Override
    public void handleMessage(NefitProtos.ServerToClientManufacturer message)
    {

    }

//    try
//    {
//        String read = this.in.readLine();
//        String[] fields = read.split(" ");
//        if(fields.length != 5)
//        {
//            this.prompt.printWarning("Maybe forgot one or more elements");
//        }
//        else
//        {
//            for(NefitProtos.DisponibilityN aux: this.itemsAvailable)
//                if(aux.getNameP().equals(fields[0]))
//                {
//                    this.prompt.printWarning("You already have this product available");
//                    continue;
//                }
//            if(Integer.parseInt(fields[1]) < 1)
//            {
//                this.prompt.printWarning("Your product need at least one of minimum quantity");
//            }
//            else if(Float.parseFloat(fields[3]) < 0)
//            {
//                this.prompt.printWarning("Your product need a positive unit price");
//            }
//            else if(Integer.parseInt(fields[4]) < 1)
//            {
//                this.prompt.printWarning("Your product need a positive period to receive orders");
//            }
//            else {
//                NefitProtos.DisponibilityS disp = this.messages.createDisponibilityS(
//                    this.name, fields[0], Integer.parseInt(fields[1]), Integer.parseInt(fields[2]), Float.parseFloat(fields[3]), Integer.parseInt(fields[4])
//                );
//                NefitProtos.Server server = NefitProtos.Server.newBuilder().setM1(disp).build();
//                Client.writeDelimited(this.os, server);
//            }
//        }
//    }
//            catch (IOException e) {
//    this.prompt.printError("Something went wrong");
//}
//            catch (NumberFormatException e)
//    {
//        this.prompt.printError("Maybe the order of the fields are incorrect");
//    }
}
