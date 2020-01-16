package nefit.catalog.resources;

import com.codahale.metrics.annotation.Timed;
import nefit.catalog.representations.Users;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.util.List;

@Path("/users")
@Produces(MediaType.APPLICATION_JSON)
public class UsersResource
{
    @GET
    @Timed
    public Users getAllUsers()
    {
        return new Users(
            List.of("importer1", "importer2", "importer3"),
            List.of("manufacturer1", "manufacturer2", "manufacturer3")
        );
    }
}
