package nefit.client;

import javafx.util.Pair;

import java.io.*;

public class Manufacturer implements Runnable
{
    private String name;
    private String pass;
    private BufferedReader in;
    private PrintWriter out;
    private InputStream is;
    private OutputStream os;
    private Messages messages;

    public Manufacturer(Pair<String,String> auth, BufferedReader in, PrintWriter out, InputStream is, OutputStream os, Messages messages)
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
        out.println("Type: <Product Name> <Min Quantity> <Max Quantity> <Min Price> <Period>");
        out.println("Period is the time in seconds that negotiation will be available");
        out.flush();

        new Thread(this::receive).start();

        while(true)
        {
            try
            {
                String read = in.readLine();
                String[] fields = read.split(" ");
                if(fields.length != 5)
                {
                    out.println("Maybe forgot one or more elements");
                    out.flush();
                }
                else
                {
                    NefitProtos.Disponibility disp = this.messages.createDisponibilityS(
                     this.name,fields[0],Integer.parseInt(fields[1]),Integer.parseInt(fields[2]),Integer.parseInt(fields[3]),Integer.parseInt(fields[4]),
                     );
                    disp.writeDelimitedTo(this.os);
                }
            }
            catch (IOException e) {
                out.println("Something went wrong on read :(");
                out.flush();
            }
            catch (NumberFormatException e)
            {
                out.println("Maybe the order of the fields are incorrect");
                out.flush();
            }
        }
    }

    public void receive()
    {
        while(true)
        {
            NefitProtos.ProductionM prod = NefitProtos.ProductionM.parseDelimitedFrom(this.is);
            if(prod.getQuant() == 0)
                out.println("No good offers to your product " + prod.getNameP());
            else out.println("Your product " + prod.getNameP() + " gives you " + (prod.getValue() * prod.getQuant()) + " M.U.");
            out.flush();
        }
    }

}
