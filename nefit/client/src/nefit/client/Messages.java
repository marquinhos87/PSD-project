package nefit.client;

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
    public MsgAuth createMsgAuth(Boolean typeAuth, Boolean client, String name, String pass)
    {
        MsgAuth msg = MsgAuth.newBuilder()
            .setName(name)
            .setPass(pass);
        if(typeAuth && client)
        {
            return msg.setCtype(MsgAuth.ClientType.MANUFACTOR)
                .setMtype(MsgAuth.MsgType.LOGIN).build();
        }
        if(typeAuth)
        {
            return msg.setCtype(MsgAuth.ClientType.IMPORTER)
                .setMtype(MsgAuth.MsgType.LOGIN).build();
        }
        if(client)
        {
            return msg.setCtype(MsgAuth.ClientType.MANUFACTOR)
                .setMtype(MsgAuth.MsgType.REGISTER).build();
        }
        return msg.setCtype(MsgAuth.ClientType.IMPORTER)
            .setMtype(MsgAuth.MsgType.REGISTER).build();
    }

    /**
     *
     * @param nameM Name of the Manufactor
     * @param nameP Name of the Product that Manufator will produce
     * @param minimun Minimun quantity to produce
     * @param maximun Maximun quantity to produce
     * @param value Minimun value to produce
     * @param period Time that negotiation will be available
     * @return Message with the product info
     */
    public DisponibilityS createDisponibilityS(String nameM, String nameP, int minimun, int maximun, int value, int period)
    {
        return DisponibilityS.newBuilder()
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
     * @param nameM Name of the Manufactor
     * @param nameP Name of the Product
     * @param quantity Quantity to purchase
     * @param value Price per unit
     * @return Message with the order to a product
     */
    public OrderS createOrderS(String nameM, String nameP, int quantity, int value)
    {
        return OrderS.newBuilder()
            .setNameM(nameM)
            .setNameP(nameP)
            .setQuant(quantity)
            .setValue(value)
            .build();
    }

    /**
     *
     * @param subs List of the Names of Manufactors that Importer want to subscribe
     * @return Message with the Names of MAnufactors
     */
    public SubS createSubS(List<String> subs)
    {
        return SubS.newBuilder().setSubs(subs).build();
    }
}
