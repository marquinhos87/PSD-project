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
        if(typeAuth && !client)
        {
            return msg.setCtype(NefitProtos.MsgAuth.ClientType.IMPORTER)
                .setMtype(NefitProtos.MsgAuth.MsgType.LOGIN).build();
        }
        if(!typeAuth && client)
        {
            return msg.setCtype(NefitProtos.MsgAuth.ClientType.MANUFACTURER)
                .setMtype(NefitProtos.MsgAuth.MsgType.REGISTER).build();
        }
        return msg.setCtype(NefitProtos.MsgAuth.ClientType.IMPORTER)
            .setMtype(NefitProtos.MsgAuth.MsgType.REGISTER).build();
    }
}
