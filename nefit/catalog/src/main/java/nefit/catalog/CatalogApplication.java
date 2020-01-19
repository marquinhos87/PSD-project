package nefit.catalog;

import io.dropwizard.Application;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;
import nefit.catalog.resources.NegotiationsResource;
import nefit.catalog.resources.UsersResource;
import org.zeromq.SocketType;
import org.zeromq.ZMQ;

public class CatalogApplication
    extends Application< CatalogConfiguration >
    implements AutoCloseable
{
    private final State state;

    private final ZMQ.Context zmqContext;
    private final ZMQ.Socket zmqSocket;

    private final Thread receiveThread;

    public CatalogApplication(int zmqPort)
    {
        this.state = new State();

        this.zmqContext = ZMQ.context(1);
        this.zmqSocket = this.zmqContext.socket(SocketType.SUB);
        this.zmqSocket.bind(String.format("tcp://*:%d", zmqPort));
        this.zmqSocket.subscribe("".getBytes(ZMQ.CHARSET));

        this.receiveThread = new Thread(this::receiveLoop);
    }

    @Override
    public String getName()
    {
        return "catalog";
    }

    @Override
    public void initialize(Bootstrap< CatalogConfiguration > bootstrap)
    {
    }

    @Override
    public void run(
        CatalogConfiguration catalogConfiguration, Environment environment
    )
    {
        environment.healthChecks().register("check", new CatalogHealthCheck());
        environment.jersey().register(new UsersResource(state));
        environment.jersey().register(new NegotiationsResource(state));
    }

    @Override
    public void close() throws InterruptedException
    {
        this.zmqSocket.close();
        this.zmqContext.close();

        // terminate message receiving thread

        this.receiveThread.interrupt();
        this.receiveThread.join();
    }

    private void receiveLoop()
    {
        // receive messages until interrupted

        while (!Thread.interrupted())
        {
            try
            {
                final var topic = this.zmqSocket.recvStr();
                final var message = this.zmqSocket.recv();

                switch (topic)
                {
                    case "addUser":
                        break;

                    case "addNegotiation":
                        break;

                    case "removeNegotiation":
                        break;
                }
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
