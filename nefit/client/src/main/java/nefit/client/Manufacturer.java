package nefit.client;

import nefit.proto.NefitProtos;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class Manufacturer implements Runnable
{
    private List<NefitProtos.DisponibilityN> itemsAvailable;
    private String name;
    private BufferedReader in;
    private PrintWriter out;
    private InputStream is;
    private OutputStream os;
    private Messages messages;

    public Manufacturer(String name, BufferedReader in, PrintWriter out, InputStream is, OutputStream os, Messages messages)
    {
        this.name = name;
        this.in = in;
        this.out = out;
        this.is = is;
        this.os = os;
        this.messages = messages;
        this.itemsAvailable = new ArrayList<>();
    }

    @Override
    public void run()
    {
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
                }
                else
                {
                    for(NefitProtos.DisponibilityN aux: itemsAvailable)
                        if(aux.getNameP().equals(fields[0]))
                        {
                            out.println("You already have this product available");
                            continue;
                        }
                    NefitProtos.DisponibilityS disp = this.messages.createDisponibilityS(
                     this.name,fields[0],Integer.parseInt(fields[1]),Integer.parseInt(fields[2]),Float.parseFloat(fields[3]),Integer.parseInt(fields[4])
                     );
                    disp.writeDelimitedTo(this.os);
                }
            }
            catch (IOException e) {
                out.println("Something went wrong");
            }
            catch (NumberFormatException e)
            {
                out.println("Maybe the order of the fields are incorrect");
            }
            finally
            {
                out.flush();
            }
        }
    }

    public void receive()
    {
        while(true)
        {
            try {
                NefitProtos.ProductionM prod = NefitProtos.ProductionM.parseDelimitedFrom(this.is);
                if (prod.getQuant() == 0)
                    out.println("No good offers to your product " + prod.getNameP());
                else
                    out.println("Your product " + prod.getNameP() + " gives you " + (prod.getValue() * prod.getQuant()) + " M.U.");
                for(NefitProtos.DisponibilityN aux: itemsAvailable)
                    if(aux.getNameP().equals(prod.getNameP()))
                    {
                        itemsAvailable.remove(aux);
                        break;
                    }
            }
            catch (IOException e)
            {
                out.println("Something went wrong");
            }
            finally
            {
                out.flush();
            }
        }
    }

}
