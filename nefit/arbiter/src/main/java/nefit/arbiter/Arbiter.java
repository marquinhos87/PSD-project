package nefit.arbiter;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

public class Arbiter implements Runnable
{
    //List<Negociation> negociations;
    private ZMQ.Socket socket;
    private ZContext context;
    private Messages messages;

    public Arbiter(){}

    public static void main(String[] args)
    {
        new Arbiter().run();
    }

    @Override
    public void run()
    {
        this.messages = new Messages();
        this.context = new ZContext();
        this.socket = context.createSocket(SocketType.REQ);
        this.socket.connect("tcp://localhost:12345");

        new Thread(this::receive).start();

        //TODO
    }

    private void receive()
    {
        while(true)
        {

        }
    }
}
