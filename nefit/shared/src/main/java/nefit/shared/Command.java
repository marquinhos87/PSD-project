package nefit.shared;

public class Command
{
    private final String commandName;
    private final String[] argumentNames;

    public Command(
        String commandName,
        String... argumentNames
    )
    {
        this.commandName = commandName;
        this.argumentNames = argumentNames;
    }

    public String getCommandName()
    {
        return this.commandName;
    }

    public String[] getArgumentNames()
    {
        return this.argumentNames;
    }
}
