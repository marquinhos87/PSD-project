package nefit.client;

import com.google.protobuf.Parser;
import nefit.shared.Command;
import nefit.shared.Connection;
import nefit.shared.Prompt;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public abstract class Client< MessageType >
{
    private final Prompt prompt;
    private final Connection connection;
    private final String username;

    private final Parser< MessageType > messageParser;
    private final Map< String, Command > commands;
    private final String knownCommandsMessage;

    private final Thread receiveThread;

    public Client(
        Prompt prompt,
        Connection connection,
        String username,
        Parser< MessageType > messageParser,
        Command... commands
    )
    {
        this.prompt = prompt;
        this.connection = connection;
        this.username = username;

        this.messageParser = messageParser;

        this.commands = Arrays.stream(commands).collect(
            Collectors.toUnmodifiableMap(Command::getCommandName, c -> c)
        );

        this.knownCommandsMessage =
            Arrays
                .stream(commands)
                .map(c -> '"' + c.getCommandName() + '"')
                .collect(Collectors.joining(", "));

        this.receiveThread = new Thread(this::receiveLoop);
    }

    public Prompt getPrompt()
    {
        return this.prompt;
    }

    public Connection getConnection()
    {
        return this.connection;
    }

    public String getUsername()
    {
        return this.username;
    }

    public void run() throws IOException, InterruptedException
    {
        // start message receiving thread

        this.receiveThread.start();

        // run input loop

        while (true)
        {
            final var commandName = this.prompt.input("> ");

            if (commandName == null)
                break; // no more input

            final var command = this.commands.get(commandName);

            if (command == null)
            {
                this.prompt.printError(
                    "Unknown command \"%s\". Try %s.",
                    commandName,
                    this.knownCommandsMessage
                );

                continue;
            }

            final var arguments = new ArrayList< String >();

            for (final var argumentName : command.getArgumentNames())
                arguments.add(prompt.input(argumentName + ": "));

            try
            {
                this.handleCommand(commandName, arguments);
            }
            catch (Exception e)
            {
                this.prompt.printError(e.getMessage());
            }
        }

        // close connection

        this.connection.close();

        // terminate message receiving thread

        this.receiveThread.interrupt();
        this.receiveThread.join();
    }

    abstract protected void handleCommand(String command, List< String > args)
        throws IOException;

    abstract protected void handleMessage(MessageType message)
        throws IOException;

    private void receiveLoop()
    {
        // receive messages until interrupted

        while (!Thread.interrupted())
        {
            try
            {
                final var message = this.connection.receive(this.messageParser);

                this.prompt.print();
                this.handleMessage(message);
                this.prompt.printNoNewline("> ");
            }
            catch (Throwable t)
            {
                if (Thread.interrupted())
                    break; // ignore exception if connection closed
                else
                    throw new RuntimeException(t);
            }
        }
    }
}
