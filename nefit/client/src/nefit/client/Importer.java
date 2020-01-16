package nefit.client;

import javafx.util.Pair;

import java.io.PrintWriter;
import java.io.BufferedReader;

public class Importer implements Runnable {
    private String name;
    private String pass;
    private BufferedReader in;
    private PrintWriter out;

    public Importer(Pair<String,String> auth, BufferedReader in, PrintWriter out)
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
