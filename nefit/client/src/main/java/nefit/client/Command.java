package nefit.client;

public class Command
{
    private final String name;
    private final int numArgs;

    public Command(String name, int numArgs)
    {
        this.name = name;
        this.numArgs = numArgs;
    }

    public String getName()
    {
        return this.name;
    }

    public int getNumArgs()
    {
        return this.numArgs;
    }
}
