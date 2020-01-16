package nefit.catalog;

import com.codahale.metrics.health.HealthCheck;

public class CatalogHealthCheck extends HealthCheck
{
    @Override
    protected Result check()
    {
        return Result.healthy();
    }
}
