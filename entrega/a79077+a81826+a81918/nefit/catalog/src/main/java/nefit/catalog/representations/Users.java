package nefit.catalog.representations;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class Users
{
    @JsonProperty
    public final List< String > importers;

    @JsonProperty
    public final List< String > manufacturers;

    public Users(
        Set< String > importerUsernames,
        Set< String > manufacturerUsernames
    )
    {
        this.importers =
            importerUsernames
                .stream()
                .sorted()
                .collect(Collectors.toUnmodifiableList());

        this.manufacturers =
            manufacturerUsernames
                .stream()
                .sorted()
                .collect(Collectors.toUnmodifiableList());
    }
}
