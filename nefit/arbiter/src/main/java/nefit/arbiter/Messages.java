package nefit.arbiter;

import nefit.proto.NefitProtos;

import java.util.List;

public class Messages {

    public Messages() {}

    /**
     *
     * @param ack If true Order accepted, false otherwise
     * @param msg Extra info
     * @param nameI Name of the Importer
     * @return Message with the Info of a proposed order
     */
    public NefitProtos.OrderAckS createOrderAckS(Boolean ack, String msg, String nameI, Boolean outdated)
    {
        if(msg == null)
        {
            return NefitProtos.OrderAckS.newBuilder()
                .setAck(ack)
                .setNameI(nameI)
                .setOutdated(outdated)
                .build();
        }
        return NefitProtos.OrderAckS.newBuilder()
            .setAck(ack)
            .setMsg(msg)
            .setNameI(nameI)
            .setOutdated(outdated)
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
    public NefitProtos.ProductionS createProductionS(String nameM, String nameP, int quantity, float value)
    {
        return NefitProtos.ProductionS.newBuilder()
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
    public NefitProtos.ResultS createResultS(Boolean ack, String msg, String nameI)
    {
        if(msg == null)
        {
            return NefitProtos.ResultS.newBuilder()
                .setResult(ack)
                .setNameI(nameI)
                .build();
        }
        return NefitProtos.ResultS.newBuilder()
            .setResult(ack)
            .setMsg(msg)
            .setNameI(nameI)
            .build();
    }

    public NefitProtos.InfoS createInfoS(String nameM, String nameP, int maximum, int minimum, float value, int period, String nameI)
    {
        return NefitProtos.InfoS.newBuilder()
            .setNameM(nameM)
            .setNameP(nameP)
            .setMaximum(maximum)
            .setMinimum(minimum)
            .setValue(value)
            .setPeriod(period)
            .setNameI(nameI)
            .build();
    }
}
