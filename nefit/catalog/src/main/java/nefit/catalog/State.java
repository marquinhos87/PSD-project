package nefit.catalog;

import nefit.catalog.representations.Negotiation;
import nefit.shared.NefitProto;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

public class State
{
    private static class NegotiationKey implements Comparable<NegotiationKey>
    {
        public final String manufacturerName;
        public final String productName;

        public NegotiationKey(String manufacturerName, String productName)
        {
            this.manufacturerName = manufacturerName;
            this.productName = productName;
        }

        @Override
        public boolean equals(Object o) {
            if(o == null) return false;
            if(o.getClass() != this.getClass()) return false;
            NegotiationKey other = (NegotiationKey) o;
            return other.manufacturerName.equals(this.manufacturerName) && other.productName.equals(this.productName);
        }

        @Override
        public int hashCode() {
            return Objects.hash(manufacturerName,productName);
        }

        @Override
        public int compareTo(NegotiationKey other)
        {
            final var i = this.manufacturerName.compareTo(other.manufacturerName);

            if (i != 0)
                return i;
            else
                return this.productName.compareTo(other.productName);
        }
    }

    private final Set< String > importerUsernames;
    private final Set< String > manufacturerUsernames;

    private final Map< NegotiationKey, Negotiation > negotiations;

    public State()
    {
        this.importerUsernames = new HashSet<>();
        this.manufacturerUsernames = new HashSet<>();

        this.negotiations = new HashMap<>();
    }

    public synchronized Set< String > getImporterUsernames()
    {
        return Set.copyOf(this.importerUsernames);
    }

    public synchronized Set< String > getManufacturerUsernames()
    {
        return Set.copyOf(this.manufacturerUsernames);
    }

    public synchronized List< Negotiation > getNegotiations()
    {
        return List.copyOf(negotiations.values());
    }

    public synchronized void addImporter(String importerUsername)
    {
        this.importerUsernames.add(importerUsername);
    }

    public synchronized void addManufacturer(String manufacturerUsername)
    {
        this.manufacturerUsernames.add(manufacturerUsername);
    }

    public synchronized void addNegotiation(Negotiation negotiation)
    {
        this.negotiations.put(
            new NegotiationKey(
                negotiation.manufacturerName,
                negotiation.productName
            ),
            negotiation
        );
    }

    public void removeNegotiation(String manufacturerName, String productName)
    {
        this.negotiations.remove(
            new NegotiationKey(manufacturerName, productName)
        );
    }
}
