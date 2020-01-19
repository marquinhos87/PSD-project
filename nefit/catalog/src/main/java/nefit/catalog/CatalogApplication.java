package nefit.catalog;

import com.codahale.metrics.health.HealthCheck;
import io.dropwizard.Application;
import io.dropwizard.Configuration;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;
import nefit.catalog.representations.Negotiation;
import nefit.catalog.resources.NegotiationsResource;
import nefit.catalog.resources.UsersResource;
import nefit.shared.NefitProto;
import org.zeromq.SocketType;
import org.zeromq.ZMQ;

public class CatalogApplication
    extends Application< Configuration >
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
        this.zmqSocket.connect(String.format("tcp://*:%d", zmqPort));
        this.zmqSocket.subscribe("".getBytes(ZMQ.CHARSET));

        this.receiveThread = new Thread(this::receiveLoop);
    }

    @Override
    public String getName()
    {
        return "catalog";
    }

    @Override
    public void initialize(Bootstrap< Configuration > bootstrap)
    {
    }

    @Override
    public void run(Configuration configuration, Environment environment)
    {
        environment.healthChecks().register("check", new HealthCheck()
        {
            @Override
            protected Result check()
            {
                return Result.healthy();
            }
        });

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
                final var messageBytes = this.zmqSocket.recv();

                switch (topic)
                {
                    case "addUser":
                        handleMessage(
                            NefitProto.FrontendToCatalogAddUser
                                .parseFrom(messageBytes)
                        );
                        break;

                    case "addNegotiation":
                        handleMessage(
                            NefitProto.ArbiterToCatalogAddNegotiation
                                .parseFrom(messageBytes)
                        );
                        break;

                    case "removeNegotiation":
                        handleMessage(
                            NefitProto.ArbiterToCatalogRemoveNegotiation
                                .parseFrom(messageBytes)
                        );
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

    private void handleMessage(NefitProto.FrontendToCatalogAddUser msg)
    {
        switch (msg.getType())
        {
            case IMPORTER:
                this.state.addImporter(msg.getUsername());
                break;

            case MANUFACTURER:
                this.state.addManufacturer(msg.getUsername());
                break;
        }
    }

    private void handleMessage(NefitProto.ArbiterToCatalogAddNegotiation msg)
    {
        this.state.addNegotiation(
            new Negotiation(
                msg.getManufacturerName(),
                msg.getProductName(),
                msg.getMinQuantity(),
                msg.getMaxQuantity(),
                msg.getMinUnitPrice()
            )
        );
    }

    private void handleMessage(NefitProto.ArbiterToCatalogRemoveNegotiation msg)
    {
        this.state.removeNegotiation(
            msg.getManufacturerName(),
            msg.getProductName()
        );
    }
}
