package nefit.client;

import javafx.util.Pair;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

public class Manufactor implements Runnable
{
    //List<Article> articles;
    private String name;
    private String pass;
    private BufferedReader in;
    private PrintWriter out;
    private ZContext context;
    private ZMQ.Socket socket;
    private Messages messages;

    public Manufactor(Pair<String,String> auth, BufferedReader in, PrintWriter out, ZContext context, ZMQ.Socket socket, Messages messages)
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
