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
        printCommands();
        new Thread(this::receiveNotification).start();
        new Thread(this::receiveResult).start();
        //TODO
        while(true)
        {
            String command = in.readLine();
            String[] fields = command.split(" ");
            if(fields.length < 1)
            {
                out.println("You don't write anything");
                out.flush();
            }
            else
            {
                switch (fields[0]) {
                    case "sub":
                        if (fields.length == 1)
                        {
                            out.println("Forgot Names of Manufacturers")
                            out.flush();
                        }
                        else
                        {
                            NefitProtos.
                        }
                        break;

                    case "get":
                        if (fields.length != 1)
                        {
                            out.println("To 'get' type only 'get'");
                            out.flush();
                        }
                        else
                        {
                            NefitProtos.GetS get = this.messages.createGetS();
                            get.writeDelimitedTo(this.os);

                            NefitProtos.NegotiationsI nego = NefitProtos.parseDelimitedFrom(this.is);
                            for(NefitProtos.InfoI aux: nego.getNegotiations())
                            {
                                out.println("Product available");
                                out.println("\tManufacturer: " + info.getNameM());
                                out.println("\tProduct: " + info.getNameP());
                                out.println("\tMin quantity: " + info.getMinimun());
                                out.println("\tMax quantity: " + info.getMaximun());
                                out.println("\tMin unit price: " + info.getValue());
                                out.println("\tTime Available: " + info.getPeriod() + " seconds");
                            }
                            out.flush();
                        }
                        break;

                    case "order":
                        if(fields.length != 5)
                        {
                            out.println("Maybe forgot something");
                            out.flush();
                        }
                        else
                        {
                            try
                            {
                                NefitProtos.OrderS order = this.messages.createOrderS(
                                    fields[1],fields[2],Integer.parseInt(fields[3]),Integer.parseInt(fields[5])
                                );
                                order.writeDelimitedTo(this.os);

                                NefitProtos.OrderAckI ack = NefitProtos.OrderAckI.parseDelimitedFrom(this.is);
                                if(ack.getAck())
                                {
                                    out.println("Order accepted");
                                }
                                else
                                {
                                    out.println("Order decline, because: "+ack.getMsg());
                                }
                            }
                            catch (IOException e)
                            {
                                out.println("Something went wrong, try again");
                            }
                            catch (NumberFormatException e)
                            {
                                out.println("Quantity and Price have to be a number");
                            }
                            finally
                            {
                                out.flush();
                            }
                        }
                        break;

                    default:
                        out.println("Wrong command, try one of this:");
                        printCommands();
                        break;
                }
            }
        }
    }

    public void receiveNotification()
    {
        while (true)
        {
            NefitProtos.InfoI info = NefitProtos.InfoI.parseDelimitedFrom(this.is);
            out.println("New Product available");
            out.println("\tManufacturer: " + info.getNameM());
            out.println("\tProduct: " + info.getNameP());
            out.println("\tMin quantity: " + info.getMinimun());
            out.println("\tMax quantity: " + info.getMaximun());
            out.println("\tMin unit price: " + info.getValue());
            out.println("\tTime Available: " + info.getPeriod() + " seconds");
            out.flush();
        }
    }

    public void receiveResult()
    {
        while(true)
        {
            NefitProtos.ResultI result = NefitProtos.ResultI.parseDelimitedFrom(this.is);
            if(result.getAck())
            {
                out.println("You win the order " + result.getMsg());
            }
            else
            {
                out.println("You lose the order " + result.getMsg());
            }
            out.flush();
        }
    }

    public void printCommands()
    {
        out.println("You can use some commands like 'sub' 'get' 'order'");
        out.println("Command sub: <sub> <Name Manufacturer> [<Name Manufacturer>] ...");
        out.println("Command get: <get>");
        out.println("Command order: <order> <Name Manufacturer> <Name Product> <Quantity> <Unit Price>")
        out.flush();
    }
}
