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
        Prompt prompt = new Prompt(new PrintWriter(new OutputStreamWriter(System.out)));

        Pair<Pair<String,String>,Pair<String,String>> arg = parseArgs(args);

        if(arg == null)
        {
            prompt.printError("Usage: <type> <auth> <name> <pass>");
            prompt.printError("<type> is 'm' or 'i'");
            prompt.printError("<auth> is 'l' or 'r'");
            System.exit(2);
        }

        try
        {
            Messages messages = new Messages();
            Socket socket = new Socket("localhost",12345);
            InputStream is = socket.getInputStream();
            OutputStream os = socket.getOutputStream();

            //Authentication
            if(arg.getKey().getValue().equals("l"))
            {
                if(!Login(arg.getValue(),messages,is,os))
                {
                    prompt.printError("Invalid data, shutting down");
                    System.exit(3);
                }
            }
            else
            {
                if(Register(arg.getValue(),messages,is,os))
                {
                    prompt.printOthers("Fez o Registo com sucesso");
                    if(!Login(arg.getValue(),messages,is,os))
                    {
                        prompt.printError("Something went wrong, shutting down");
                        System.exit(3);
                    }
                }
                else
                {
                    prompt.printError("Manufactor/Importer yet registered, trying Login");
                    if(!Login(arg.getValue(),messages,is,os))
                    {
                        prompt.printError("Something went wrong, shutting down");
                        System.exit(3);
                    }
                }
            }
            prompt.printOthers("Conseguiu fazer login");
            if (arg.getKey().getValue().equals("m"))
                new Manufacturer(arg.getValue().getKey(),in,is,os,messages,prompt).run();
            else
                new Importer(arg.getValue().getKey(),in,is,os,messages,prompt).run();
        }
        catch (IOException e)
        {
            prompt.printError("Something went wrong");
            System.exit(4);
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
        NefitProtos.MsgAuth msgl;
        if (arg.getKey().equals("m"))
            msgl = messages.createMsgAuth(true,true,arg.getKey(),arg.getValue());
        else
            msgl = messages.createMsgAuth(true,false,arg.getKey(),arg.getValue());

        msgl.writeDelimitedTo(os);
        os.flush();

        //Wait for MsgAck
        NefitProtos.MsgAck ack = NefitProtos.MsgAck.parseDelimitedFrom(is);

        return ack.getOk();
    }

    private static Boolean Register(Pair<String,String> arg, Messages messages, InputStream is, OutputStream os) throws IOException {
        NefitProtos.MsgAuth msgl;
        if (arg.getKey().equals("m"))
            msgl = messages.createMsgAuth(false,true,arg.getKey(),arg.getValue());
        else
            msgl = messages.createMsgAuth(false,false,arg.getKey(),arg.getValue());

        byte[] aux = msgl.toByteArray();
        msgl.writeDelimitedTo(os);
        os.flush();

        //Wait for MsgAck
        NefitProtos.MsgAck ack = NefitProtos.MsgAck.parseDelimitedFrom(is);

        return ack.getOk();
    }
}
