package nefit.client;

import nefit.proto.NefitProtos;

import java.io.IOException;

public class Main
{
    public static void main(String[] args) throws IOException
    {
        try (final var prompt = new Prompt())
        {
            final Connection connection;

            try
            {
                connection = new Connection(args[0], Integer.parseInt(args[1]));
            }
            catch (Exception e)
            {
                prompt.fail(e.getMessage());
                return;
            }

            final var clientType = authenticate(connection, prompt);

            switch (clientType)
            {
                case IMPORTER:
                    prompt.print("You are now authenticated as an Importer.");
                    new Importer(connection, prompt).run();
                    break;

                case MANUFACTURER:
                    prompt.print(
                        "You are now authenticated as an Manufacturer."
                    );
                    new Manufacturer(connection, prompt).run();
                    break;
            }
        }
    }

    private static NefitProtos.ClientType authenticate(
        Connection connection, Prompt prompt
    ) throws IOException
    {

        switch (prompt.input("Login or Register [l/r]: "))
        {
            case "l":
                login(connection, prompt);
                break;

            case "m":

                switch (prompt.input("Importer or Manufacturer [i/m]: "))
                {
                    case "i":
                        register(
                            connection, prompt,
                            NefitProtos.ClientType.IMPORTER
                        );
                        break;

                    case "m":
                        register(
                            connection, prompt,
                            NefitProtos.ClientType.MANUFACTURER
                        );
                        break;

                    default:
                        prompt.fail("Invalid choice.");
                }

                break;

            default:
                prompt.fail("Invalid choice.");
        }

        final var authMsg = connection
            .receive(NefitProtos.ServerToClientAuth.parser());

        if (!authMsg.getOk())
        {
            prompt.fail(authMsg.getErrorMessage());
        }

        return authMsg.getClientType();
    }

    private static void login(Connection connection, Prompt prompt)
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
    }

    private static void register(
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
