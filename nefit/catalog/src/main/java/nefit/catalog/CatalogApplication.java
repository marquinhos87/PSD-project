package nefit.catalog;

import io.dropwizard.Application;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;
import nefit.catalog.resources.UsersResource;

public class CatalogApplication extends Application< CatalogConfiguration >
{
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
    public void run(CatalogConfiguration catalogConfiguration, Environment environment)
    {
        environment.healthChecks().register("placeholder", new CatalogHealthCheck());
        environment.jersey().register(new UsersResource());
    }
}
