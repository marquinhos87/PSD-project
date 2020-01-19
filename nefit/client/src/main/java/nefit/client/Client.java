package nefit.client;

import com.google.protobuf.MessageLite;
import com.google.protobuf.Parser;

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

    abstract protected void handleCommand(String command, List< String > args)
        throws IOException;

    abstract protected void handleMessage(MessageType message)
        throws IOException;

    public void run(Prompt prompt) throws IOException, InterruptedException
    {
        // start message receiving thread

        this.receiveThread.start();

        // run input loop

        while (true)
        {
            final var commandName = prompt.input("> ");

            if (commandName == null)
                break;

            final var command = this.commands.get(commandName);

            if (command == null)
            {
                prompt.printError("Unknown command.");
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
                prompt.printError(e.getMessage());
            }
        }

        // close connection

        this.connection.close();

        // terminate message receiving thread

        this.receiveThread.interrupt();
        this.receiveThread.join();
    }

    protected void sendMessage(MessageLite message) throws IOException
    {
        this.connection.send(message);
    }

    private void receiveLoop()
    {
        // receive messages until interrupted

        while (!Thread.interrupted())
        {
            try
            {
                final var message = this.connection.receive(this.messageParser);
                this.handleMessage(message);
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

//    private static Boolean Register(
//        String type, Pair< String, String > arg, Messages messages,
//        InputStream is, OutputStream os
//    ) throws IOException
//    {
//        NefitProtos.MsgAuth msgl;
//        if (type.equals("m"))
//            msgl = messages
//                .createMsgAuth(false, true, arg.getKey(), arg.getValue());
//        else
//            msgl = messages
//                .createMsgAuth(false, false, arg.getKey(), arg.getValue());
//        writeDelimited(os, msgl);
//    }

}
