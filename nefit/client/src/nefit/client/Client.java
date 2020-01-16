package nefit.client;

import javafx.util.Pair;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.io.*;

public class Client
{
    public static void main(String[] args)
    {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        PrintWriter out = new PrintWriter(new OutputStreamWriter(System.out));

        Pair<String,Pair<String,String>> arg = parseArgs(args);

        if(arg == null)
        {
            out.println("Usage: <type> <name> <pass>");
            out.println("<type> is 'm' or 'i'");
            out.flush();
            System.exit(2);
        }
        Messages messages = new Messages();
        ZContext context = new ZContext();
        ZMQ.Socket socket = context.createSocket(SocketType.REQ);
        //Replace '5555' by servers port
        socket.connect("tcp://localhost:5555");

        //Authentication
        while(true)
        {
            try
            {
                out.println("Login(l) or Register and Login(r)");
                out.flush();
                String auth = in.readLine();
                if(auth.equals("l"))
                {
                    //TODO : MsgAuth Login
                    //if (arg.getKey().equals("m"))
                    //MsgAuth msgl = messages.createMsgAuth(true,true,this.name,this.pass);
                    //else
                    //MsgAuth msgl = messages.createMsgAuth(true,false,this.name,this.pass);

                    //socket.send(msgl.getBytes(ZMQ.CHARSET), 0);

                    //Wait for MsgAck
                    //byte[] reply = socket.recv(0);

                    //Replace 'true' by MsgAck.getAck();
                    if(true) break;
                    else
                    {
                        out.println("Invalid data");
                        out.flush();
                    }
                }
                else
                {
                    //TODO : MsgAuth Register
                    //if (arg.getKey().equals("m"))
                    //MsgAuth msgl = messages.createMsgAuth(false,true,this.name,this.pass);
                    //else
                    //MsgAuth msgl = messages.createMsgAuth(false,false,this.name,this.pass);

                    //socket.send(msgl.getBytes(ZMQ.CHARSET), 0);

                    //Wait for MsgAck
                    //byte[] reply = socket.recv(0);

                    //Replace 'true' by MsgAck.getAck();
                    if(true)
                    {
                        //TODO : MsgAuth Login
                        //if (arg.getKey().equals("m"))
                        //MsgAuth msgl = messages.createMsgAuth(true,true,this.name,this.pass);
                        //else
                        //MsgAuth msgl = messages.createMsgAuth(true,false,this.name,this.pass);

                        //socket.send(msgl.getBytes(ZMQ.CHARSET), 0);

                        //Wait for MsgAck
                        //byte[] reply = socket.recv(0);

                        //Replace 'true' by MsgAck.getAck();
                        if(true) break;
                    }
                    else
                    {
                        out.println("Manufactor/Importer yet registered");
                        out.flush();
                    }
                }

            }
            catch (IOException e)
            {
                e.printStackTrace();
            }
        }

        if (arg.getKey().equals("m"))
            new Manufactor(arg.getValue(),in,out,context,socket,messages).run();
        else
            new Importer(arg.getValue(),in,out,context,socket,messages).run();
    }

    private static Pair<String,Pair<String,String>> parseArgs(String[] args)
    {
        if(args.length != 3 && (!args[0].equals("m") || !args[0].equals("i")))
            return null;
        Pair<String,Pair<String,String>> arg = new Pair<>(args[0],new Pair<>(args[1],args[2]));
        return arg;
    }
}
