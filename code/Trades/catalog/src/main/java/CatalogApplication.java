import io.dropwizard.Application;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;

public class CatalogApplication extends Application<CatalogConfiguration> {

    public static void main(String[] args) {
        try {
            new CatalogApplication().run(args);
        }
        catch (Exception e) {

        }
    }

    @Override
    public void initialize(Bootstrap<CatalogConfiguration> bootstrap) {

    }

    @Override
    public void run(CatalogConfiguration catalogConfiguration, Environment environment) throws Exception {

    }
}