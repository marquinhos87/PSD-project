package nefit.client;

import nefit.proto.NefitProtos;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class Importer implements Runnable
{
    private String name;
    private BufferedReader in;
    private InputStream is;
    private OutputStream os;
    private Messages messages;
    private Prompt prompt;

    public Importer(String name, BufferedReader in, InputStream is, OutputStream os, Messages messages,Prompt prompt)
    {
        this.name = name;
        this.in = in;
        this.is = is;
        this.os = os;
        this.messages = messages;
        this.prompt = prompt;
    }

    @Override
    public void run()
    {
        this.prompt.printOthers(printCommands());
        new Thread(this::receive).start();
        while(true)
        {
            try {
                String command = this.in.readLine();
                String[] fields = command.split(" ");
                if (fields.length < 1)
                {
                    this.prompt.printWarning("You don't write anything");
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
                            this.prompt.printWarning("Wrong command, try one of this:");
                            this.prompt.printOthers(printCommands());
                            break;
                    }
                }
            }
            catch (IOException e)
            {
                this.prompt.printError("Something went wrong");
            }
        }
    }

    private void sendSub(String[] fields) throws IOException
    {
        if (fields.length == 1)
        {
            this.prompt.printWarning("Forgot Names of Manufacturers");
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
            this.prompt.printWarning("To 'get' type only 'get'");
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
            this.prompt.printWarning("Maybe forgot something");
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
                this.prompt.printError("Quantity and Price have to be a number");
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
                    this.prompt.printMessages("New Product available:");
                    this.prompt.printMessages(printInfo(importer.getInfo()));
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
                this.prompt.printError("Something went wrong");
            }
        }
    }

    private String printInfo(NefitProtos.InfoI info)
    {
        StringBuilder sb = new StringBuilder();
        sb.append("\tManufacturer: " + info.getNameM());
        sb.append("\n\tProduct: " + info.getNameP());
        sb.append("\n\tMin quantity: " + info.getMinimun());
        sb.append("\n\tMax quantity: " + info.getMaximun());
        sb.append("\n\tMin unit price: " + info.getValue());
        sb.append("\n\tTime Available: " + info.getPeriod() + " seconds");
        return  sb.toString();
    }

    private void printResult(NefitProtos.ResultI result)
    {
        if(result.getResult())
        {
            this.prompt.printMessages("You win the order " + result.getMsg());
        }
        else
        {
            this.prompt.printMessages("You lose the order " + result.getMsg());
        }
    }

    private void printOrderAck(NefitProtos.OrderAckI ack)
    {
        if(ack.getOutdated())
        {
            this.prompt.printMessages(ack.getMsg());
        }
        if(ack.getAck())
        {
            this.prompt.printMessages("Order accepted");
        }
        else
        {
            this.prompt.printMessages("Order decline, because: "+ack.getMsg());
        }
    }

    private void printNegotiations(NefitProtos.NegotiationsI negotiation)
    {
        for (NefitProtos.InfoI info : negotiation.getNegotiationsList()) {
            this.prompt.printMessages("Product available:");
            this.prompt.printMessages(printInfo(info));
        }
    }

    private String printCommands()
    {
        StringBuilder sb = new StringBuilder();
        sb.append("You can use some commands like 'sub' 'get' 'order'\n");
        sb.append("Command sub: <sub> <Name Manufacturer> [<Name Manufacturer>] ...\n");
        sb.append("Command get: <get>\n");
        sb.append("Command order: <order> <Name Manufacturer> <Name Product> <Quantity> <Unit Price>");
        return sb.toString();
    }
}
