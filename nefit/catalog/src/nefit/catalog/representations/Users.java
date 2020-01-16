package nefit.catalog.representations;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Collection;
import java.util.List;

public class Users
{
    @JsonProperty
    public final List< String > importers;

    @JsonProperty
    public final List< String > manufacturers;

    public Users(Collection< String > importers, Collection< String > manufacturers)
    {
        this.importers = List.copyOf(importers);
        this.manufacturers = List.copyOf(manufacturers);
    }
}
