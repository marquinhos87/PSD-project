package nefit.client;

import javafx.util.Pair;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;

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

        if (arg.getKey().equals("m"))
            new Manufactor(arg.getValue(),in,out).run();
        else
            new Importer(arg.getValue(),in,out).run();
    }

    private static Pair<String,Pair<String,String>> parseArgs(String[] args)
    {
        if(args.length != 3 && (!args[0].equals("m") || !args[0].equals("i")))
            return null;
        Pair<String,Pair<String,String>> arg = new Pair<>(args[0],new Pair<>(args[1],args[2]));
        return arg;
    }
}
