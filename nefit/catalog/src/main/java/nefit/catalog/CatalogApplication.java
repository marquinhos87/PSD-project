package nefit.catalog;

import io.dropwizard.Application;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;
import nefit.catalog.resources.NegotiationsResource;
import nefit.catalog.resources.UsersResource;

public class CatalogApplication
    extends Application< CatalogConfiguration >
    implements AutoCloseable
{
    private final State state;

    public CatalogApplication(int zeromqPort)
    {
        this.state = new State();
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
        environment.healthChecks()
            .register("placeholder", new CatalogHealthCheck());
        environment.jersey().register(new UsersResource(state));
        environment.jersey().register(new NegotiationsResource(state));
    }

    @Override
    public void close()
    {

    }
}
