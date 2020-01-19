package nefit.client;

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

    public void run(Prompt prompt) throws IOException, InterruptedException
    {
        // start message receiving thread

        this.receiveThread.start();

        // run input loop

        while (true)
        {
            final var commandName = prompt.input("> ");

            if (commandName == null)
                break; // no more input

            final var command = this.commands.get(commandName);

            if (command == null)
            {
                prompt.printError(
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
                prompt.printError(e.getMessage());
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

    //    public static void main(String[] args)
//    {
//        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
//        Prompt prompt = new Prompt(new PrintWriter(new OutputStreamWriter(System.out)));
//
//        Pair<Pair<String,String>,Pair<String,String>> arg = parseArgs(args);
//
//        if(arg == null)
//        {
//            prompt.printError("Usage: <type> <auth> <name> <pass>");
//            prompt.printError("<type> is 'm' or 'i'");
//            prompt.printError("<auth> is 'l' or 'r'");
//            System.exit(2);
//        }
//
//        try
//        {
//            Messages messages = new Messages();
//            Socket socket = new Socket("localhost",12345);
//            InputStream is = socket.getInputStream();
//            OutputStream os = socket.getOutputStream();
//
//            //Authentication
//            if(arg.getKey().getKey().equals("l"))
//            {
//                if(!Login(arg.getKey().getKey(),arg.getValue(),messages,is,os))
//                {
//                    prompt.printError("Invalid data, shutting down");
//                    System.exit(3);
//                }
//            }
//            else
//            {
//                if(Register(arg.getKey().getKey(),arg.getValue(),messages,is,os))
//                {
//                    prompt.printOthers("Fez o Registo com sucesso");
//                    if(!Login(arg.getKey().getKey(),arg.getValue(),messages,is,os))
//                    {
//                        prompt.printError("Something went wrong, shutting down");
//                        System.exit(3);
//                    }
//                }
//                else
//                {
//                    prompt.printError("Manufacturer/Importer yet registered, trying Login");
//                    if(!Login(arg.getKey().getKey(),arg.getValue(),messages,is,os))
//                    {
//                        prompt.printError("Something went wrong, shutting down");
//                        System.exit(3);
//                    }
//                }
//            }
//            prompt.printOthers("Conseguiu fazer login");
//            if (arg.getKey().getKey().equals("m"))
//                new Manufacturer(arg.getValue().getKey(),in,is,os,messages,prompt).run();
//            else
//                new Importer(arg.getValue().getKey(),in,is,os,messages,prompt).run();
//        }
//        catch (IOException e)
//        {
//            prompt.printError("Something went wrong");
//            System.exit(4);
//        }
//    }
//
//    private static Pair<Pair<String,String>,Pair<String,String>> parseArgs(String[] args)
//    {
//        if(args.length != 4)
//            return null;
//        if(!args[0].equals("m") && !args[0].equals("i"))
//            return null;
//        if(!args[1].equals("l") && !args[1].equals("r"))
//            return null;
//        Pair<Pair<String,String>,Pair<String,String>> arg = new Pair<>(new Pair<>(args[0],args[1]),new Pair<>(args[2],args[3]));
//        return arg;
//    }
//
//    private static Boolean Login(String type, Pair<String,String> arg, Messages messages, InputStream is, OutputStream os) throws IOException {
//        NefitProtos.MsgAuth msgl;
//        if (type.equals("m"))
//            msgl = messages.createMsgAuth(true,true,arg.getKey(),arg.getValue());
//        else
//            msgl = messages.createMsgAuth(true,false,arg.getKey(),arg.getValue());
//        writeDelimited(os, msgl);
//
//        //Wait for MsgAck
//        final var ack = parseDelimited(is, NefitProtos.MsgAck.parser());
//
//        return ack.getOk();
//    }
//
//    private static Boolean Register(String type, Pair<String,String> arg, Messages messages, InputStream is, OutputStream os) throws IOException {
//        NefitProtos.MsgAuth msgl;
//        System.out.println(type);
//        if (type.equals("m"))
//            msgl = messages.createMsgAuth(false,true,arg.getKey(),arg.getValue());
//        else
//            msgl = messages.createMsgAuth(false,false,arg.getKey(),arg.getValue());
//        writeDelimited(os, msgl);
//
//        //Wait for MsgAck
//        final var ack = parseDelimited(is, NefitProtos.MsgAck.parser());
//
//        return ack.getOk();
//    }
}
