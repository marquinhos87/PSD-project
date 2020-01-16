package nefit.arbiter;

public class Messages {

    public Messages() {}

    /**
     *
     * @param ack If true Order accepted, false otherwise
     * @param msg Extra info
     * @param nameI Name of the Importer
     * @return Message with the Info of a proposed order
     */
    public OrderAckS createOrderAckS(Boolean ack, String msg, String nameI)
    {
        if(msg == null)
        {
            return OrderAckS.newBuilder()
                .setAck(ack)
                .setNameI(nameI)
                .build();
        }
        return OrderAckS.newBuilder()
            .setAck(ack)
            .setMsg(msg)
            .setNameI(nameI)
            .build();
    }

    /**
     *
     * @param nameM Name of the Manufacturer
     * @param nameP Name of the Product
     * @param quantity Quantity to produce
     * @param value Value per unit
     * @return Message with the Info to Manufacturer produce his Product
     */
    public ProductionS createProductionS(String nameM, String nameP, int quantity, int value)
    {
        return ProductionS.newBuilder()
            .setNameM(nameM)
            .setNameP(nameP)
            .setQuant(quantity)
            .setValue(value)
            .build();
    }

    /**
     *
     * @param ack If true Importer win the negotiation, false otherwise
     * @param msg Extra Info
     * @param nameI Name of the Importer
     * @return Message with the Result of one Negotiation
     */
    public ResultS createResultS(Boolean ack, String msg, String nameI)
    {
        if(msg == null)
        {
            return ResultS.newBuilder()
                .setAck(ack)
                .setNameI(nameI)
                .build();
        }
        return ResultS.newBuilder()
            .setAck(ack)
            .setMsg(msg)
            .setNameI(nameI)
            .build();
    }
}
