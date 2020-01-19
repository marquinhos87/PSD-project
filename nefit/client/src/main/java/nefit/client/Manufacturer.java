package nefit.client;

import nefit.proto.NefitProtos;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

public class Manufacturer
    extends Client< NefitProtos.ProductionM >
{
    private final Set< String > activeProductNames;

    public Manufacturer(Connection connection, String username)
    {
        super(
            connection,
            username,
            NefitProtos.ProductionM.parser(),
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
    public void handleMessage(NefitProtos.ProductionM message)
    {
        if (message.getQuant() == 0)
        {
            this.printNotice(
                "No offers received for product \"%s\". Announcement removed.",
                message.getNameP()
            );
        }
        else
        {
            this.printNotice(
                "Sold %d units of product \"%s\" at a unit price of %.2f," +
                    " totalling a price of %.2f. Announcement removed.",
                message.getQuant(),
                message.getNameP(),
                message.getValue(),
                message.getQuant() * message.getValue()
            );
        }

        this.activeProductNames.remove(message.getNameP());
    }
}
