package nefit.client;

import nefit.proto.NefitProtos;

import java.io.IOException;
import java.net.InetSocketAddress;

public class Main
{
    private static InetSocketAddress parseArgs(Prompt prompt, String[] args)
    {
        try
        {
            Util.ensure(args.length == 2);

            return new InetSocketAddress(args[0], Integer.parseInt(args[1]));
        }
        catch (Exception e)
        {
            prompt.print("Usage: nefit-client <server_host> <server_port>");
            System.exit(2);
            return null;
        }
    }

    private static Connection connectToServer(
        Prompt prompt, InetSocketAddress serverEndpoint
    )
    {
        try
        {
            return new Connection(serverEndpoint);
        }
        catch (Exception e)
        {
            prompt.fail(e.getMessage());
            return null;
        }
    }

    public static void main(String[] args) throws IOException
    {
        try (final var prompt = new Prompt())
        {
            final var serverEndpoint = parseArgs(prompt, args);
            final var connection = connectToServer(prompt, serverEndpoint);

            authenticateClient(prompt, connection);
        }
    }

    private static void authenticateClient(
        Prompt prompt, Connection connection
    ) throws IOException
    {
        final String username;

        switch (prompt.input("Login or Register [l/r]: "))
        {
            case "l":
                username = login(connection, prompt);
                break;

            case "m":

                switch (prompt.input("Importer or Manufacturer [i/m]: "))
                {
                    case "i":
                        username = register(
                            connection, prompt,
                            NefitProtos.ClientType.IMPORTER
                        );
                        break;

                    case "m":
                        username = register(
                            connection, prompt,
                            NefitProtos.ClientType.MANUFACTURER
                        );
                        break;

                    default:
                        prompt.fail("Invalid choice.");
                        return;
                }

                break;

            default:
                prompt.fail("Invalid choice.");
                return;
        }

        final var authMsg = connection.receive(
            NefitProtos.ServerToClientAuth.parser()
        );

        if (!authMsg.getOk())
            prompt.fail(authMsg.getErrorMessage());

        switch (authMsg.getClientType())
        {
            case IMPORTER:
                prompt.print("You are now authenticated as an Importer.");
                new Importer(prompt, connection, username).run();
                break;

            case MANUFACTURER:
                prompt.print(
                    "You are now authenticated as an Manufacturer."
                );
                new Manufacturer(prompt, connection, username).run();
                break;
        }
    }

    private static String login(Connection connection, Prompt prompt)
        throws IOException
    {
        final var username = prompt.input("Username: ");
        final var password = prompt.input("Password: ");

        final var loginMsg = NefitProtos.ClientToServerLogin
            .newBuilder()
            .setUsername(username)
            .setPassword(password)
            .build();

        connection.send(loginMsg);

        return username;
    }

    private static String register(
        Connection connection, Prompt prompt, NefitProtos.ClientType clientType
    ) throws IOException
    {
        final var username = prompt.input("Username: ");
        final var password = prompt.input("Password: ");

        final var registerMsg = NefitProtos.ClientToServerRegister
            .newBuilder()
            .setUsername(username)
            .setPassword(password)
            .setClientType(clientType)
            .build();

        connection.send(registerMsg);

        return username;
    }

//    private static Function< Connection, Client > promptClientType(
//        String[] args,
//        Prompt prompt
//    ) throws IOException {
//
//        final String str;
//
//        if (args.length >= 3)
//            str = args[2];
//        else
//            str = prompt.input("Importer or Manufacturer [i/m]: ");
//
//        switch (str) {
//            case "i":
//                return null;
//            case "m":
//                return null;
//            default:
//                prompt.fail("Invalid client type.");
//                return null;
//        }
//    }
//
//    private static NefitProtos.MsgAuth.MsgType promptAuthOp(
//        String[] args,
//        Prompt prompt
//    ) throws IOException {
//
//        final String str;
//
//        if (args.length >= 4)
//            str = args[3];
//        else
//            str = prompt.input("Login or Register [l/r]: ");
//
//        switch (str) {
//            case "l":
//                return NefitProtos.MsgAuth.MsgType.LOGIN;
//            case "r":
//                return NefitProtos.MsgAuth.MsgType.REGISTER;
//            default:
//                prompt.fail("Invalid authentication operation.");
//                return null;
//        }
//    }
//
//    private static String promptUsername(
//        String[] args,
//        Prompt prompt
//    ) throws IOException {
//
//        if (args.length >= 5)
//            return args[4];
//        else
//            return prompt.input("Username: ");
//    }
//
//    private static String promptPassword(
//        String[] args,
//        Prompt prompt
//    ) throws IOException {
//
//        if (args.length >= 6)
//            return args[5];
//        else
//            return prompt.input("Password: ");
//    }
}
