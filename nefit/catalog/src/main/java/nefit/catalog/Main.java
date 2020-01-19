package nefit.catalog;

import nefit.shared.Prompt;
import nefit.shared.Util;

public class Main
{
    public static void main(String[] args) throws Exception
    {
        try (final var prompt = new Prompt())
        {
            final int zmqPort;

            try
            {
                Util.ensure(args.length == 1);
                zmqPort = Integer.parseInt(args[0]);
            }
            catch (Exception e)
            {
                prompt.print("Usage: nefit-catalog <zmq_port>");
                System.exit(2);
                return;
            }

            try (final var app = new CatalogApplication(zmqPort))
            {
                app.run(new String[] { "server" });
                prompt.input("Press ENTER to exit.");
            }

            System.exit(0);
        }
    }
}
