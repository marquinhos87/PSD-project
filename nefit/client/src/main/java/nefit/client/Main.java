package nefit.client;

import nefit.shared.Connection;
import nefit.shared.NefitProto;
import nefit.shared.Prompt;
import nefit.shared.Util;

import java.io.IOException;
import java.net.InetSocketAddress;

public class Main
{
    public static void main(String[] args)
        throws IOException, InterruptedException
    {
        try (final var prompt = new Prompt())
        {
            final var serverEndpoint = parseArgs(prompt, args);
            final var connection = connectToServer(prompt, serverEndpoint);

            authenticateClient(prompt, connection);
        }
    }

    private static InetSocketAddress parseArgs(Prompt prompt, String[] args)
    {
        try
        {
            Util.ensure(args.length == 1);

            final var parts = args[0].split(":", 2);
            Util.ensure(parts.length == 2);

            return new InetSocketAddress(parts[0], Integer.parseInt(parts[1]));
        }
        catch (Exception e)
        {
            prompt.print("Usage: nefit-client <server_host>:<server_port>");
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

    private static void authenticateClient(
        Prompt prompt, Connection connection
    ) throws IOException, InterruptedException
    {
        final String username;

        switch (prompt.input("Login or Register [l/r]: "))
        {
            case "l":
                username = login(connection, prompt);
                break;

            case "r":
                username = register(connection, prompt);
                break;

            default:
                prompt.fail("Invalid choice.");
                return;
        }

        // TODO: use newer messages

//        final var authMsg = connection.receive(
//            NefitProtos.ServerToClientAuth.parser()
//        );
//
//        if (!authMsg.getOk())
//            prompt.fail(authMsg.getErrorMessage());
//
//        switch (authMsg.getClientType())
//        {
//            case IMPORTER:
//                prompt.print("You are now authenticated as an Importer.");
//                new Importer(prompt, connection, username).run();
//                break;
//
//            case MANUFACTURER:
//                prompt.print(
//                    "You are now authenticated as an Manufacturer."
//                );
//                new Manufacturer(prompt, connection, username).run();
//                break;
//        }
    }

    private static String login(Connection connection, Prompt prompt)
        throws IOException, InterruptedException
    {
        final NefitProto.ClientType clientType;

        switch (prompt.input("Importer or Manufacturer [i/m]: "))
        {
            case "i":
                clientType = NefitProto.ClientType.IMPORTER;
                break;

            case "m":
                clientType = NefitProto.ClientType.MANUFACTURER;
                break;

            default:
                prompt.fail("Invalid choice.");
                return null;
        }

        final var username = prompt.input("Username: ");
        final var password = prompt.input("Password: ");

        // TODO: use newer messages and don't require user to enter client type

        final var loginMessage = NefitProto.ClientToServerLogin
            .newBuilder()
            .setUsername(username)
            .setPassword(password)
            .setClientType(clientType)
            .build();

        connection.send(NefitProto.ClientToServer.newBuilder().setLogin(loginMessage).build());

        final var ackMessage = connection.receive(NefitProto.ServerToClientAuth.parser());

        if (!ackMessage.getOk())
            prompt.fail(ackMessage.getErrorMessage());

        switch (clientType)
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

        return username;
    }

    private static String register(Connection connection, Prompt prompt)
        throws IOException, InterruptedException
    {
        final NefitProto.ClientType clientType;

        switch (prompt.input("Importer or Manufacturer [i/m]: "))
        {
            case "i":
                clientType = NefitProto.ClientType.IMPORTER;
                break;

            case "m":
                clientType = NefitProto.ClientType.MANUFACTURER;
                break;

            default:
                prompt.fail("Invalid choice.");
                return null;
        }

        final var username = prompt.input("Username: ");
        final var password = prompt.input("Password: ");

        // TODO: use newer messages

        final var registerMessage = NefitProto.ClientToServerRegister
            .newBuilder()
            .setUsername(username)
            .setPassword(password)
            .build();

        connection.send(NefitProto.ClientToServer.newBuilder().setRegister(registerMessage).build());

        final var ackRegister = connection.receive(NefitProto.ServerToClientAuth.parser());

        if (!ackRegister.getOk())
            prompt.fail(ackRegister.getErrorMessage());

        final var loginMessage = NefitProto.ClientToServerLogin
            .newBuilder()
            .setUsername(username)
            .setPassword(password)
            .setClientType(clientType)
            .build();

        connection.send(NefitProto.ClientToServer.newBuilder().setLogin(loginMessage).build());

        final var ackLogin = connection.receive(NefitProto.ServerToClientAuth.parser());

        if (!ackLogin.getOk())
            prompt.fail(ackLogin.getErrorMessage());

        switch (clientType)
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

        return username;
    }
}
