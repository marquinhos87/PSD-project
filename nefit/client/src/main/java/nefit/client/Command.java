package nefit.client;

import java.util.function.Consumer;

public class Command
{
    private final Consumer< String[] > action;

    private final String commandName;
    private final String[] argumentNames;

    public Command(
        Consumer< String[] > action,
        String commandName,
        String... argumentNames
    )
    {
        this.action = action;

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

    public void runAction(String[] arguments)
    {
        this.action.accept(arguments);
    }
}
