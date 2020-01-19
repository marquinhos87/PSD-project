package nefit.catalog.resources;

import com.codahale.metrics.annotation.Timed;
import nefit.catalog.State;
import nefit.catalog.representations.Negotiation;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.util.List;

@Path("/negotiations")
@Produces(MediaType.APPLICATION_JSON)
public class NegotiationsResource
{
    private final State state;

    public NegotiationsResource(State state)
    {
        this.state = state;
    }

    @GET
    @Timed
    public List< Negotiation > get()
    {
        return this.state.getNegotiations();
    }
}
