package nefit.catalog.representations;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Negotiation
{
    @JsonProperty
    public final String manufacturerName;

    @JsonProperty
    public final String productName;

    @JsonProperty
    public final int minQuantity;

    @JsonProperty
    public final int maxQuantity;

    @JsonProperty
    public final float minUnitPrice;

    public Negotiation(
        String manufacturerName,
        String productName,
        int minQuantity,
        int maxQuantity,
        float minUnitPrice
    )
    {
        this.manufacturerName = manufacturerName;
        this.productName = productName;

        this.minQuantity = minQuantity;
        this.maxQuantity = maxQuantity;

        this.minUnitPrice = minUnitPrice;
    }
}
