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
    private InputStream is;
    private OutputStream os;
    private Messages messages;
    private Prompt prompt;

    public Manufacturer(String name, BufferedReader in, InputStream is, OutputStream os, Messages messages, Prompt prompt)
    {
        this.name = name;
        this.in = in;
        this.is = is;
        this.os = os;
        this.messages = messages;
        this.prompt = prompt;
        this.itemsAvailable = new ArrayList<>();
    }

    @Override
    public void run()
    {
        prompt.printOthers("Type: <Product Name> <Min Quantity> <Max Quantity> <Min Price> <Period>");
        prompt.printOthers("Period is the time in seconds that negotiation will be available");

        new Thread(this::receive).start();

        while(true)
        {
            try
            {
                String read = in.readLine();
                String[] fields = read.split(" ");
                if(fields.length != 5)
                {
                    prompt.printWarning("Maybe forgot one or more elements");
                }
                else
                {
                    for(NefitProtos.DisponibilityN aux: itemsAvailable)
                        if(aux.getNameP().equals(fields[0]))
                        {
                            prompt.printWarning("You already have this product available");
                            continue;
                        }
                    if(Integer.parseInt(fields[1]) < 1)
                    {
                        prompt.printWarning("Your product need at least one of minimum quantity");
                    }
                    else if(Float.parseFloat(fields[3]) < 0)
                    {
                        prompt.printWarning("Your product need a positive unit price");
                    }
                    else if(Integer.parseInt(fields[4]) < 1)
                    {
                        prompt.printWarning("Your product need a positive period to receive orders");
                    }
                    else {
                        NefitProtos.DisponibilityS disp = this.messages.createDisponibilityS(
                            this.name, fields[0], Integer.parseInt(fields[1]), Integer.parseInt(fields[2]), Float.parseFloat(fields[3]), Integer.parseInt(fields[4])
                        );
                        disp.writeDelimitedTo(this.os);
                    }
                }
            }
            catch (IOException e) {
                prompt.printError("Something went wrong");
            }
            catch (NumberFormatException e)
            {
                prompt.printError("Maybe the order of the fields are incorrect");
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
                    prompt.printMessages("No good offers to your product " + prod.getNameP());
                else
                    prompt.printMessages("Your product " + prod.getNameP() + " gives you " + (prod.getValue() * prod.getQuant()) + " M.U.");
                for(NefitProtos.DisponibilityN aux: itemsAvailable)
                    if(aux.getNameP().equals(prod.getNameP()))
                    {
                        itemsAvailable.remove(aux);
                        break;
                    }
            }
            catch (IOException e)
            {
                prompt.printError("Something went wrong");
            }
        }
    }

}
