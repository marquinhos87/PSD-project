package nefit.client;

import nefit.proto.NefitProtos;

import java.util.List;

public class Messages
{
    //Empty Constructor
    public Messages(){}

    /**
     *
     * @param typeAuth If true represents Login, false represents Register
     * @param client If true represents manufactor, false represents importer
     * @param name Name of the Manufactor or Importer
     * @param pass Pass of the Manufactor or Importer
     * @return Message with parameters to Authentication
     */
    public NefitProtos.MsgAuth createMsgAuth(Boolean typeAuth, Boolean client, String name, String pass)
    {
        NefitProtos.MsgAuth.Builder msg = NefitProtos.MsgAuth.newBuilder()
            .setName(name)
            .setPass(pass);
        if(typeAuth && client)
        {
            return msg.setCtype(NefitProtos.MsgAuth.ClientType.MANUFACTURER)
                .setMtype(NefitProtos.MsgAuth.MsgType.LOGIN).build();
        }
        if(typeAuth)
        {
            return msg.setCtype(NefitProtos.MsgAuth.ClientType.IMPORTER)
                .setMtype(NefitProtos.MsgAuth.MsgType.LOGIN).build();
        }
        if(client)
        {
            return msg.setCtype(NefitProtos.MsgAuth.ClientType.MANUFACTURER)
                .setMtype(NefitProtos.MsgAuth.MsgType.REGISTER).build();
        }
        return msg.setCtype(NefitProtos.MsgAuth.ClientType.IMPORTER)
            .setMtype(NefitProtos.MsgAuth.MsgType.REGISTER).build();
    }

    /**
     *
     * @param nameM Name of the Manufactor
     * @param nameP Name of the Product that Manufacturer will produce
     * @param minimun Minimun quantity to produce
     * @param maximun Maximun quantity to produce
     * @param value Minimun value to produce
     * @param period Time that negotiation will be available
     * @return Message with the product info
     */
    public NefitProtos.DisponibilityS createDisponibilityS(String nameM, String nameP, int minimun, int maximun, int value, int period)
    {
        return NefitProtos.DisponibilityS.newBuilder()
            .setNameM(nameM)
            .setNameP(nameP)
            .setMinimun(minimun)
            .setMaximun(maximun)
            .setValue(value)
            .setPeriod(period)
            .build();
    }

    /**
     *
     * @param nameM Name of the Manufacturer
     * @param nameP Name of the Product
     * @param quantity Quantity to purchase
     * @param value Price per unit
     * @return Message with the order to a product
     */
    public NefitProtos.OrderS createOrderS(String nameM, String nameP, int quantity, int value)
    {
        return NefitProtos.OrderS.newBuilder()
            .setNameM(nameM)
            .setNameP(nameP)
            .setQuant(quantity)
            .setValue(value)
            .build();
    }

    /**
     *
     * @param subs List of the Names of Manufacturers that Importer want to subscribe
     * @return Message with the Names of Manufacturers
     */
    public NefitProtos.SubS createSubS(List<String> subs)
    {
        NefitProtos.SubS.Builder sub = NefitProtos.SubS.newBuilder();
        for(String aux: subs)
            sub.addSubs(aux);
        return sub.build();
    }
}
