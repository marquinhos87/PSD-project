package nefit.client;

import com.google.protobuf.MessageLite;
import com.google.protobuf.Parser;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

public abstract class Client< MessageType >
{
    private final Map< String, Command > commands;
    private final Parser< MessageType > messageParser;

    private final Connection connection;
    private final Thread receiveThread;

    public Client(Connection connection, Parser< MessageType > messageParser, Command[] commands)
    {
        this.commands = new HashMap<>();
        this.messageParser = messageParser;

        this.connection = connection;
        this.receiveThread = new Thread(this::receiveLoop);
    }

    abstract protected void handleCommand(String command, String[] args)
        throws Exception;

    abstract protected void handleMessage(MessageType message) throws Exception;

    public void run(Prompt prompt) throws IOException, InterruptedException
    {
        // start message receiving thread

        this.receiveThread.start();

        // run input loop

        while (true)
        {
            final var line = prompt.input("> ");

            if (line == null)
                break;

            final var args =
                Pattern
                    .compile("\\s+")
                    .splitAsStream(line);

            final var parts = line.split("\\s+", 2);

            if (parts[0].equals("exit"))
                break;

            final var command = this.commands.get(parts[0]);

            if (command == null)
            {
                prompt.printError("Unknown command.");
                continue;
            }

            final var args = new ArrayList< String >();

            if ()
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
}
