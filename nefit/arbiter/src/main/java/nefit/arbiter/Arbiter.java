package nefit.arbiter;

import com.google.protobuf.InvalidProtocolBufferException;
import javafx.util.Pair;
import nefit.shared.Connection;
import nefit.shared.NefitProto;
import nefit.shared.Prompt;
import nefit.shared.Util;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Arbiter implements Runnable
{
    private ZMQ.Socket socketZ;
    private ZContext context;
    private Connection connection;
    private Prompt prompt;

    private NefitProto.DisponibilityN disp;

    /**
     * Map where the Key is a Pair with the name of the Manufacturer and name of the Product
     * The Value for this Key is a Pair with the information of the product and the best order to the product
     * The best order to the product can be 'null' if doesn't exist a order that satisfies the requirements of the product
     * Map<NameManufacturer,Pair<Product,Best Order>>
     */
    private Map<Pair<String,String>,Pair< NefitProto.DisponibilityN, NefitProto.OrderN>> negotiations;

    /**
     * Map where the Key is a Pair with the name of the Manufacturer and name of the Product
     * The Value for this Key is the list of all importers that make at least one offer to the product
     * Map<Pair<NameManufacturer,Product>,List<NameImporter>>
     */
    private Map<Pair<String,String>,List<String>> importerNego;

    /**
     * Map where the Key is the name of the Importer
     * The Value for this Key is the list of all Manufacturers names that the Importer is interested
     * Map<NameImporter,List<NameManufacturer>>
     */
    private Map<String,List<String>> subscribers;

    /**
     * Map where the Key is the name of the Manufacturer
     * The Value for this Key is the list of all Importers names that are interested in all products of this Manufacturer
     * Map<NameManufacturer,List<NameImporter>>
     */
    private Map<String,List<String>> importerManu;

    /**
     * Empty constructor
     */
    public Arbiter(Prompt prompt, Connection connection){
        this.prompt = prompt;
        this.connection = connection;
    }

    public static void main(String[] args) throws IOException, InterruptedException
    {
        try (final var prompt = new Prompt())
        {
            final var serverEndpoint = parseArgs(prompt, args);
            final var connection = connectToServer(prompt, serverEndpoint);
            new Arbiter(prompt, connection).run();
        }
    }

    private static Connection connectToServer(
        Prompt prompt, InetSocketAddress serverEndpoint
    )
    {
        try
        {
            return new Connection(serverEndpoint);
        }
        catch (Exception e)
        {
            prompt.fail(e.getMessage());
            return null;
        }
    }

    private static InetSocketAddress parseArgs(Prompt prompt, String[] args)
    {
        try
        {
            Util.ensure(args.length == 1);

            final var parts = args[0].split(":", 2);
            Util.ensure(parts.length == 2);

            return new InetSocketAddress(parts[0], Integer.parseInt(parts[1]));
        }
        catch (Exception e)
        {
            prompt.print("Usage: nefit-client <server_host>:<server_port>");
            System.exit(2);
            return null;
        }
    }

    @Override
    public void run()
    {
        this.context = new ZContext();
        this.socketZ = context.createSocket(SocketType.SUB);
        this.socketZ.connect("tcp://localhost:12346");

        this.negotiations = new HashMap<>();
        this.subscribers = new HashMap<>();
        this.importerNego = new HashMap<>();
        this.importerManu = new HashMap<>();

        new Thread(this::receivedZMQ).start();

        while(true)
        {
            try
            {
                final var negotiator = connection.receive(NefitProto.ServerToArbiter.parser());
                //final var negotiator = connection.receive(is, NefitProto.Negotiator.parser());

                if(negotiator.hasAnnounce())
                    executeDisponibility(negotiator.getAnnounce());

                if(negotiator.hasSubscribe())
                    executeSub(negotiator.getSubscribe());
            }
            catch (InvalidProtocolBufferException e) {
                e.printStackTrace();
            }
            catch (IOException e){
                e.printStackTrace();
            }
        }
    }

    private void receivedZMQ(){
        while (true){
            try {
                byte[] reply = this.socketZ.recv(0);
                //Não funciona neste momento
                NefitProto.ServerToArbiter negotiator = NefitProto.ServerToArbiter.parseFrom(reply);

                if(negotiator.hasOffer())
                    executeOrder(negotiator.getOffer());

            } catch (InvalidProtocolBufferException e) {
                e.printStackTrace();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    private synchronized void executeDisponibility(NefitProto.ServerToArbiterAnnounce disponibility) throws IOException {
        this.negotiations.put(new Pair<>(disponibility.getNameM(),disponibility.getNameP()),new Pair<>(disponibility, null));
        this.importerNego.put(new Pair<>(disponibility.getNameM(),disponibility.getNameP()),new ArrayList<>());
        this.disp = disponibility;

        //Thread that leads to
        new Thread(this::executeResult).start();

        this.socketZ.subscribe(disponibility.getNameM()+disponibility.getNameP());

        if(!this.importerManu.containsKey(disponibility.getNameM()))
            this.importerManu.put(disponibility.getNameM(),new ArrayList<>());
        else
            for(String str: this.importerManu.get(disponibility.getNameM())) {
                NefitProto.InfoS infoS = this.messages.createInfoS(
                                            disponibility.getNameM(),
                                            disponibility.getNameP(),
                                            disponibility.getMaximum(),
                                            disponibility.getMinimum(),
                                            disponibility.getValue(),
                                            disponibility.getPeriod(),
                                            str);
                //Client.writeDelimited(os,NefitProto.Server.newBuilder().setM5(infoS).build());
                NefitProto.Server.newBuilder().setM5(infoS).build().writeDelimitedTo(os);
            }
    }

    private synchronized void executeOrder(NefitProto.ServerToArbiterOffer order) throws IOException {
        NefitProto.OrderAckS ack;
        if(this.negotiations.containsKey(new Pair<>(order.getNameM(),order.getNameP())))
        {
            Pair< NefitProto.DisponibilityN, NefitProto.OrderN> aux = this.negotiations.get(new Pair<>(order.getNameM(),order.getNameP()));
            if (aux.getKey().getMaximum() >= order.getQuant() && aux.getKey().getMinimum() <= order.getQuant() && aux.getKey().getValue() <= order.getValue())
            {
                float old_v = aux.getValue().getValue() * aux.getValue().getQuant();
                float new_v = order.getValue() * order.getQuant();
                if (new_v > old_v) {
                    //Send ack to old importer
                    NefitProto.OrderAckS orderAckS = this.messages.createOrderAckS(false, "Your order to " + order.getNameM() + ":" + order.getNameP() + "is outdated", aux.getValue().getNameI(), true);
                    //Client.writeDelimited(os, NefitProto.Server.newBuilder().setM7(orderAckS).build());
                    NefitProto.Server.newBuilder().setM7(orderAckS).build().writeDelimitedTo(os);
                    ack = this.messages.createOrderAckS(true, null, order.getNameI(), false);
                    //Replace Order
                    this.negotiations.put(new Pair<>(order.getNameM(),order.getNameP()),new Pair<>(aux.getKey(),order));
                }
                else
                {
                    ack = this.messages.createOrderAckS(false,"Exists a better order",order.getNameI(),false);
                }
            } else {
                ack = this.messages.createOrderAckS(false, "Don't have the minimums required or the quantity is very high", order.getNameI(), false);
            }
            if(!this.importerNego.get(new Pair<>(order.getNameM(),order.getNameP())).contains(order.getNameI()))
                this.importerNego.get(new Pair<>(order.getNameM(),order.getNameP())).add(order.getNameI());
            //Client.writeDelimited(os, NefitProto.Server.newBuilder().setM7(ack).build());
            NefitProto.Server.newBuilder().setM7(ack).build().writeDelimitedTo(os);
        }
    }

    private synchronized void executeSub(NefitProto.ServerToArbiterSubscribe sub)
    {
        this.subscribers.put(sub.getNameI(),sub.getSubsList().subList(0,sub.getSubsCount()));
        for(String str: sub.getSubsList().subList(0,sub.getSubsCount())) {
            if (this.importerManu.containsKey(str)) {
                if (!this.importerManu.get(str).contains(sub.getNameI()))
                    this.importerManu.get(str).add(sub.getNameI());
            }
            else {
                List<String> aux = new ArrayList<>();
                aux.add(sub.getNameI());
                this.importerManu.put(str,aux);
            }
        }
    }

    private synchronized void executeResult() {
        NefitProto.DisponibilityN disp = NefitProto.DisponibilityN.newBuilder(this.disp).build();
        try {
            Thread.sleep(disp.getPeriod()*1000L);
            Pair<String,String> prod = new Pair<>(disp.getNameM(),disp.getNameP());
            if(!(this.negotiations.get(prod).getValue() == null))
            {
                NefitProto.OrderN order = this.negotiations.get(prod).getValue();
                NefitProto.ResultS resultS = this.messages.createResultS(true,order.getNameM() + ":" + order.getNameP(),order.getNameI());
                //Client.writeDelimited(os, NefitProto.Server.newBuilder().setM4(resultS).build());
                NefitProto.Server.newBuilder().setM4(resultS).build().writeDelimitedTo(os);
                for(String str: this.importerNego.get(prod)) {
                    if (!str.equals(order.getNameI())) {
                        resultS =  this.messages.createResultS(false, order.getNameM() + ":" + order.getNameP(), str);
                        //Client.writeDelimited(os, NefitProto.Server.newBuilder().setM4(resultS).build());
                        NefitProto.Server.newBuilder().setM4(resultS).build().writeDelimitedTo(os);
                    }
                }
                NefitProto.ProductionS productionS = this.messages.createProductionS(
                    prod.getKey(),prod.getValue(),order.getQuant(),order.getValue()
                );
                //Client.writeDelimited(os, NefitProto.Server.newBuilder().setM6(productionS).build());
                NefitProto.Server.newBuilder().setM6(productionS).build().writeDelimitedTo(os);
            }
            else
            {
                NefitProto.ProductionS productionS = this.messages.createProductionS(
                    prod.getKey(),prod.getValue(),0,0
                );
                //Client.writeDelimited(os,NefitProto.Server.newBuilder().setM6(productionS).build());
                NefitProto.Server.newBuilder().setM6(productionS).build().writeDelimitedTo(os);
            }
            this.socketZ.unsubscribe(prod.getKey()+prod.getValue());
            this.negotiations.remove(prod);
            this.importerNego.remove(prod);
        } catch (InterruptedException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
