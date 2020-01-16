package nefit.client;

import javafx.util.Pair;
import nefit.proto.NefitProtos;

import java.io.*;
import java.net.Socket;

public class Client
{
    public static void main(String[] args)
    {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        PrintWriter out = new PrintWriter(new OutputStreamWriter(System.out));

        Pair<Pair<String,String>,Pair<String,String>> arg = parseArgs(args);

        if(arg == null)
        {
            out.println("Usage: <type> <auth> <name> <pass>");
            out.println("<type> is 'm' or 'i'");
            out.println("<auth> is 'l' or 'r'");
            out.flush();
            System.exit(2);
        }


        try {
            Messages messages = new Messages();
            Socket socket = new Socket("localhost",5555);
            InputStream is = socket.getInputStream();
            OutputStream os = socket.getOutputStream();

            //Authentication
            if(arg.getKey().getValue().equals("l"))
            {
                if(Login(arg.getValue(),messages,is,os)) ;
                else
                {
                    out.println("Invalid data");
                    out.flush();
                }
            }
            else
            {
                if(Register(arg.getValue(),messages,is,os))
                {
                    if(Login(arg.getValue(),messages,is,os)) ;
                    else
                    {
                        out.println("Something went wrong, shutting down");
                        out.flush();
                        System.exit(3);
                    }
                }
                else
                {
                    out.println("Manufactor/Importer yet registered, trying Login");
                    out.flush();
                    if(Login(arg.getValue(),messages,is,os)) ;
                    else
                    {
                        out.println("Something went wrong, shutting down");
                        out.flush();
                        System.exit(3);
                    }
                }
            }
            if (arg.getKey().getValue().equals("m"))
                new Manufacturer(arg.getValue(),in,out,is,os,messages).run();
            else
                new Importer(arg.getValue(),in,out,is,os,messages).run();
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
    }

    private static Pair<Pair<String,String>,Pair<String,String>> parseArgs(String[] args)
    {
        if(args.length != 4)
            return null;
        if(!args[0].equals("m") && !args[0].equals("i"))
            return null;
        if(!args[1].equals("l") && !args[1].equals("r"))
            return null;
        Pair<Pair<String,String>,Pair<String,String>> arg = new Pair<>(new Pair<>(args[0],args[1]),new Pair<>(args[2],args[3]));
        return arg;
    }

    private static Boolean Login(Pair<String,String> arg, Messages messages, InputStream is, OutputStream os) throws IOException {
        //TODO : MsgAuth Login
        NefitProtos.MsgAuth msgl = null;
        if (arg.getKey().equals("m"))
            msgl = messages.createMsgAuth(true,true,arg.getKey(),arg.getValue());
        else
            msgl = messages.createMsgAuth(true,false,arg.getKey(),arg.getValue());

        msgl.writeDelimitedTo(os);

        //Wait for MsgAck
        NefitProtos.MsgAck ack = NefitProtos.MsgAck.parseDelimitedFrom(is);

        return ack.getAck();
    }

    private static Boolean Register(Pair<String,String> arg, Messages messages, InputStream is, OutputStream os) throws IOException {
        //TODO : MsgAuth Register
        NefitProtos.MsgAuth msgl = null;
        if (arg.getKey().equals("m"))
            msgl = messages.createMsgAuth(false,true,arg.getKey(),arg.getValue());
        else
            msgl = messages.createMsgAuth(false,false,arg.getKey(),arg.getValue());

        msgl.writeDelimitedTo(os);

        //Wait for MsgAck
        NefitProtos.MsgAck ack = NefitProtos.MsgAck.parseDelimitedFrom(is);

        return ack.getAck();
    }
}
