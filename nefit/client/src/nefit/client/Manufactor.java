package nefit.client;

import javafx.util.Pair;

import java.io.BufferedReader;
import java.io.PrintWriter;

public class Manufactor implements Runnable
{
    //List<Article> articles;
    private String name;
    private String pass;
    private BufferedReader in;
    private PrintWriter out;

    public Manufactor(Pair<String,String> auth, BufferedReader in, PrintWriter out)
    {
        this.name = auth.getKey();
        this.pass = auth.getValue();
        this.in = in;
        this.out = out;
    }

    @Override
    public void run()
    {
        //TODO
    }
}
