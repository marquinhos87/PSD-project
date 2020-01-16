package nefit.client;

import javafx.util.Pair;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import java.io.PrintWriter;
import java.io.BufferedReader;

public class Importer implements Runnable
{
    private String name;
    private String pass;
    private BufferedReader in;
    private PrintWriter out;
    private ZContext context;
    private ZMQ.Socket socket;
    private Messages messages;

    public Importer(Pair<String,String> auth, BufferedReader in, PrintWriter out, ZContext context, ZMQ.Socket socket, Messages messages)
    {
        this.name = auth.getKey();
        this.pass = auth.getValue();
        this.in = in;
        this.out = out;
        this.context = context;
        this.socket = socket;
        this.messages = messages;
    }

    @Override
    public void run()
    {
        //TODO
        while(true)
        {

        }
    }
}
