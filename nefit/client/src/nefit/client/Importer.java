package nefit.client;

import javafx.util.Pair;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.BufferedReader;

public class Importer implements Runnable
{
    private String name;
    private String pass;
    private BufferedReader in;
    private PrintWriter out;
    private InputStream is;
    private OutputStream os;
    private Messages messages;

    public Importer(Pair<String,String> auth, BufferedReader in, PrintWriter out, InputStream is, OutputStream os, Messages messages)
    {
        this.name = auth.getKey();
        this.pass = auth.getValue();
        this.in = in;
        this.out = out;
        this.is = is;
        this.os = os;
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
