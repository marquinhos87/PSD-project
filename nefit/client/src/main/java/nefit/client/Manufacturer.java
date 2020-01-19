package nefit.client;

import nefit.proto.NefitProtos;

public class Manufacturer
    extends Client< NefitProtos.ServerToClientManufacturer >
{
    public Manufacturer(Connection connection)
    {
        super(
            connection,
            NefitProtos.ServerToClientManufacturer.parser(),
            new Command[] {
                new Command(
                    this::handleCommandOffer,
                    "offer",
                    "Product name",
                    "Min. quantity",
                    "Max. quantity",
                    "Min. unit price",
                    "Timeout (seconds)"
                )
            }
        );
    }

    private void handleCommandOffer(String[] arguments)
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
                "Maximum quantity is less than minimum."
            );
        }

        final var minUnitPrice = Float.parseFloat(arguments[3]);

        if (minUnitPrice <= 0)
        {
            throw new IllegalArgumentException(
                "Minimum unit price must be positive."
            );
        }

        final var timeout = Float.parseFloat(arguments[4]);

        if (minUnitPrice <= 0)
            throw new IllegalArgumentException("Timeout must be positive.");
    }

    @Override
    protected void inputLoop(Prompt prompt, String command, String[] arguments)
    {


        switch (command)
        {
            case "announce":
        }
    }

    @Override
    public void handleMessage(NefitProtos.ServerToClientManufacturer message)
    {

    }
}
