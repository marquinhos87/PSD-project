package nefit.client;

import nefit.proto.NefitProtos;

public class Manufacturer
    extends Client< NefitProtos.ServerToClientManufacturer >
{
    public Manufacturer(Connection connection)
    {
        super(
            connection,
            NefitProtos.ServerToClientManufacturer.parser()
        );
    }

    @Override
    protected void inputLoop(Prompt prompt)
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
