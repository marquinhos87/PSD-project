package nefit.client;

import nefit.proto.NefitProtos;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

public class Manufacturer
    extends Client< NefitProtos.ServerToClientManufacturer >
{
    private final Set< String > activeProductNames;

    public Manufacturer(Connection connection, String username)
    {
        super(
            connection,
            username,
            NefitProtos.ServerToClientManufacturer.parser(),
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

    private void handleCommand(String command, String[] arguments)
        throws IOException
    {
        // validate and parse arguments (we know command is "announce")

        final var productName = arguments[0];

        Util.ensure(!productName.isBlank(), "Invalid product name.");

        Util.ensure(
            !this.activeProductNames.contains(productName),
            "A product with this name is currently available from you."
        );

        final var minQuantity = Integer.parseInt(arguments[1]);
        final var maxQuantity = Integer.parseInt(arguments[2]);

        Util.ensure(
            minQuantity > 0 && maxQuantity > 0,
            "Quantities must be positive."
        );

        Util.ensure(
            maxQuantity >= minQuantity,
            "Maximum quantity must not be lower than minimum."
        );

        final var minUnitPrice = Float.parseFloat(arguments[3]);

        Util.ensure(minUnitPrice > 0, "Minimum unit price must be positive.");

        final var timeout = Integer.parseInt(arguments[4]);

        Util.ensure(timeout > 0, "Timeout must be positive.");

        // send announcement request to server

        final var messageAnnounce =
            NefitProtos.DisponibilityS
                .newBuilder()
                .setNameM(this.getUsername())
                .setNameP(productName)
                .setMinimum(minQuantity)
                .setMaximum(maxQuantity)
                .setValue(minUnitPrice)
                .setPeriod(timeout)
                .build();

        final var messageServer =
            NefitProtos.Server
                .newBuilder()
                .setM1(messageAnnounce)
                .build();

        this.sendMessage(messageServer);
    }

    @Override
    public void handleMessage(NefitProtos.ServerToClientManufacturer message)
    {
        final var prod = nefit.client.prev.Client
            .parseDelimited(this.is, NefitProtos.ProductionM.parser());
        if (prod.getQuant() == 0)
            this.prompt.printMessages(
                "No good offers to your product " + prod.getNameP());
        else
            this.prompt.printMessages(
                "Your product " + prod.getNameP() + " gives you " + (prod
                    .getValue() * prod.getQuant()) + " M.U.");
        for (NefitProtos.DisponibilityN aux : this.itemsAvailable)
            if (aux.getNameP().equals(prod.getNameP()))
            {
                this.itemsAvailable.remove(aux);
                break;
            }

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
