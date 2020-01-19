package nefit.client;

public class Importer {
    private final Connection connection;
    private final Prompt prompt;

    public Importer(Prompt prompt, Connection connection, String username)
    {
        this.connection = connection;
        this.prompt = prompt;
    }

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
            NefitProtos.SubS sub = this.messages.createSubS(this.name,topics);
            Client.writeDelimited(this.os, NefitProtos.Server.newBuilder().setM3(sub).build());
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
                    this.name,fields[1], fields[2], Integer.parseInt(fields[3]), Float.parseFloat(fields[4])
                );
                Client.writeDelimited(this.os, NefitProtos.Server.newBuilder().setM2(order).build());
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
                final var importer = Client.parseDelimited(this.is, NefitProtos.Importer.parser());

                if(importer.hasInfo())
                {
                    this.prompt.printMessages("New Product available:");
                    this.prompt.printMessages(printInfo(importer.getInfo()));
                }

                if(importer.hasResult())
                    printResult(importer.getResult());

                if(importer.hasOrdack())
                    printOrderAck(importer.getOrdack());

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
        sb.append("\n\tMin quantity: " + info.getMinimum());
        sb.append("\n\tMax quantity: " + info.getMaximum());
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

    private String printCommands()
    {
        StringBuilder sb = new StringBuilder();
        sb.append("You can use some commands like 'sub' 'order'\n");
        sb.append("Command sub: <sub> <Name Manufacturer> [<Name Manufacturer>] ...\n");
        sb.append("Command order: <order> <Name Manufacturer> <Name Product> <Quantity> <Unit Price>");
        return sb.toString();
    }
}
