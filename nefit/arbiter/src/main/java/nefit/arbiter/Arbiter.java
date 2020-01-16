package nefit.arbiter;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

public class Arbiter
{
    //List<Negociation> negociations;

    public static void main(String[] args)
    {
        Messages messages = new Messages();
        ZContext context = new ZContext();
        ZMQ.Socket socket = context.createSocket(SocketType.REQ);
        //Replace '5555' by servers port
        socket.connect("tcp://localhost:5555");

        System.out.println("Hello from the Java arbiter.");
    }
}
