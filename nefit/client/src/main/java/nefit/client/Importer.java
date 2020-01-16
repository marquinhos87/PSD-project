package nefit.client;

import nefit.proto.NefitProtos;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class Importer implements Runnable
{
    private String name;
    private BufferedReader in;
    private PrintWriter out;
    private InputStream is;
    private OutputStream os;
    private Messages messages;

    public Importer(String name, BufferedReader in, PrintWriter out, InputStream is, OutputStream os, Messages messages)
    {
        this.name = name;
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
        out.flush();
        new Thread(this::receive).start();
        while(true)
        {
            try {
                String command = in.readLine();
                String[] fields = command.split(" ");
                if (fields.length < 1)
                {
                    out.println("You don't write anything");
                }
                else
                {
                    switch (fields[0])
                    {
                        case "sub":
                            sendSub(fields);
                            break;

                        case "get":
                            sendGet(fields);
                            break;

                        case "order":
                            sendOrder(fields);
                            break;

                        default:
                            out.println("Wrong command, try one of this:");
                            printCommands();
                            break;
                    }
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

    private void sendSub(String[] fields) throws IOException
    {
        if (fields.length == 1)
        {
            out.println("Forgot Names of Manufacturers");
        }
        else
        {
            List<String> topics = new ArrayList<>();
            for(int i = 1 ; i < fields.length ; i++)
                topics.add(fields[i]);
            NefitProtos.SubS sub = this.messages.createSubS(topics);
            sub.writeDelimitedTo(this.os);
        }
    }

    private void sendGet(String[] fields) throws IOException
    {
        if (fields.length != 1)
        {
            out.println("To 'get' type only 'get'");
        }
        else
        {
            NefitProtos.GetS get = this.messages.createGetS(this.name);
            get.writeDelimitedTo(this.os);
        }
    }

    private void sendOrder(String[] fields) throws IOException
    {
        if (fields.length != 5)
        {
            out.println("Maybe forgot something");
        }
        else
        {
            try
            {
                NefitProtos.OrderS order = this.messages.createOrderS(
                    fields[1], fields[2], Integer.parseInt(fields[3]), Float.parseFloat(fields[4])
                );
                order.writeDelimitedTo(this.os);
            }
            catch (NumberFormatException e)
            {
                out.println("Quantity and Price have to be a number");
            }
        }
    }

    private void receive()
    {
        while (true)
        {
            try
            {
                NefitProtos.Importer importer = NefitProtos.Importer.parseDelimitedFrom(this.is);

                if(importer.hasInfo())
                {
                    out.println("New Product available:");
                    printInfo(importer.getInfo());
                }

                if(importer.hasResult())
                    printResult(importer.getResult());

                if(importer.hasOrdack())
                    printOrderAck(importer.getOrdack());

                if (importer.hasNego())
                    printNegotiations(importer.getNego());

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

    private void printInfo(NefitProtos.InfoI info)
    {
        out.println("\tManufacturer: " + info.getNameM());
        out.println("\tProduct: " + info.getNameP());
        out.println("\tMin quantity: " + info.getMinimun());
        out.println("\tMax quantity: " + info.getMaximun());
        out.println("\tMin unit price: " + info.getValue());
        out.println("\tTime Available: " + info.getPeriod() + " seconds");
    }

    private void printResult(NefitProtos.ResultI result)
    {
        if(result.getResult())
        {
            out.println("You win the order " + result.getMsg());
        }
        else
        {
            out.println("You lose the order " + result.getMsg());
        }
    }

    private void printOrderAck(NefitProtos.OrderAckI ack)
    {
        if(ack.getAck())
        {
            out.println("Order accepted");
        }
        else
        {
            out.println("Order decline, because: "+ack.getMsg());
        }
    }

    private void printNegotiations(NefitProtos.NegotiationsI negotiation)
    {
        for (NefitProtos.InfoI info : negotiation.getNegotiationsList()) {
            out.println("Product available:");
            printInfo(info);
        }
    }

    private void printCommands()
    {
        out.println("You can use some commands like 'sub' 'get' 'order'");
        out.println("Command sub: <sub> <Name Manufacturer> [<Name Manufacturer>] ...");
        out.println("Command get: <get>");
        out.println("Command order: <order> <Name Manufacturer> <Name Product> <Quantity> <Unit Price>");
    }
}
