package nefit.catalog.resources;

import com.codahale.metrics.annotation.Timed;
import nefit.catalog.State;
import nefit.catalog.representations.Users;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

@Path("/users")
@Produces(MediaType.APPLICATION_JSON)
public class UsersResource
{
    private final State state;

    public UsersResource(State state)
    {
        this.state = state;
    }

    @GET
    @Timed
    public Users get()
    {
        return new Users(
            this.state.getImporterUsernames(),
            this.state.getManufacturerUsernames()
        );
    }
}
