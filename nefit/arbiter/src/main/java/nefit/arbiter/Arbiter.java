package nefit.arbiter;

import com.google.protobuf.InvalidProtocolBufferException;
import javafx.util.Pair;
import nefit.client.Client;
import nefit.proto.NefitProtos;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Arbiter implements Runnable
{
    private ZMQ.Socket socketZ;
    private ZContext context;
    private Messages messages;
    private Socket socket;
    private InputStream is;
    private OutputStream os;

    private NefitProtos.DisponibilityN disp;

    /**
     * Map where the Key is a Pair with the name of the Manufacturer and name of the Product
     * The Value for this Key is a Pair with the information of the product and the best order to the product
     * The best order to the product can be 'null' if doesn't exist a order that satisfies the requirements of the product
     * Map<NameManufacturer,Pair<Product,Best Order>>
     */
    private Map<Pair<String,String>,Pair<NefitProtos.DisponibilityN, NefitProtos.OrderN>> negotiations;

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
    public Arbiter(){}

    public static void main(String[] args)
    {
        new Arbiter().run();
    }

    @Override
    public void run()
    {
        try {
            this.messages = new Messages();
            this.context = new ZContext();
            this.socketZ = context.createSocket(SocketType.SUB);
            this.socketZ.connect("tcp://localhost:12346");

            this.socket = new Socket("localhost",12345);
            this.is = this.socket.getInputStream();
            this.os = this.socket.getOutputStream();

            this.negotiations = new HashMap<>();
            this.subscribers = new HashMap<>();
            this.importerNego = new HashMap<>();
            this.importerManu = new HashMap<>();

            new Thread(this::receivedZMQ).start();

            while(true)
            {
                try
                {
                    final var negotiator = Client.parseDelimited(is, NefitProtos.Negotiator.parser());

                    if(negotiator.hasDisponibility())
                        executeDisponibility(negotiator.getDisponibility());

                    if(negotiator.hasSub())
                        executeSub(negotiator.getSub());
                }
                catch (InvalidProtocolBufferException e) {
                    e.printStackTrace();
                }
                catch (IOException e){
                    e.printStackTrace();
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void receivedZMQ(){
        while (true){
            try {
                byte[] reply = this.socketZ.recv(0);
                NefitProtos.Negotiator negotiator = NefitProtos.Negotiator.parseFrom(reply);

                if(negotiator.hasOrder())
                    executeOrder(negotiator.getOrder());

            } catch (InvalidProtocolBufferException e) {
                e.printStackTrace();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    private synchronized void executeDisponibility(NefitProtos.DisponibilityN disponibility) throws IOException {
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
                NefitProtos.InfoS infoS = this.messages.createInfoS(
                                            disponibility.getNameM(),
                                            disponibility.getNameP(),
                                            disponibility.getMaximum(),
                                            disponibility.getMinimum(),
                                            disponibility.getValue(),
                                            disponibility.getPeriod(),
                                            str);
                Client.writeDelimited(os,NefitProtos.Server.newBuilder().setM5(infoS).build());
            }
    }

    private synchronized void executeOrder(NefitProtos.OrderN order) throws IOException {
        NefitProtos.OrderAckS ack;
        if(this.negotiations.containsKey(new Pair<>(order.getNameM(),order.getNameP())))
        {
            Pair<NefitProtos.DisponibilityN,NefitProtos.OrderN> aux = this.negotiations.get(new Pair<>(order.getNameM(),order.getNameP()));
            if (aux.getKey().getMaximum() >= order.getQuant() && aux.getKey().getMinimum() <= order.getQuant() && aux.getKey().getValue() <= order.getValue())
            {
                float old_v = aux.getValue().getValue() * aux.getValue().getQuant();
                float new_v = order.getValue() * order.getQuant();
                if (new_v > old_v) {
                    //Send ack to old importer
                    NefitProtos.OrderAckS orderAckS = this.messages.createOrderAckS(false, "Your order to " + order.getNameM() + ":" + order.getNameP() + "is outdated", aux.getValue().getNameI(), true);
                    Client.writeDelimited(os,NefitProtos.Server.newBuilder().setM7(orderAckS).build());
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
            Client.writeDelimited(os,NefitProtos.Server.newBuilder().setM7(ack).build());
        }
    }

    private synchronized void executeSub(NefitProtos.SubN sub)
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
        NefitProtos.DisponibilityN disp = NefitProtos.DisponibilityN.newBuilder(this.disp).build();
        try {
            Thread.sleep(disp.getPeriod()*1000L);
            Pair<String,String> prod = new Pair<>(disp.getNameM(),disp.getNameP());
            if(!(this.negotiations.get(prod).getValue() == null))
            {
                NefitProtos.OrderN order = this.negotiations.get(prod).getValue();
                NefitProtos.ResultS resultS = this.messages.createResultS(true,order.getNameM() + ":" + order.getNameP(),order.getNameI());
                Client.writeDelimited(os,NefitProtos.Server.newBuilder().setM4(resultS).build());
                for(String str: this.importerNego.get(prod)) {
                    if (!str.equals(order.getNameI())) {
                        resultS =  this.messages.createResultS(false, order.getNameM() + ":" + order.getNameP(), str);
                        Client.writeDelimited(os,NefitProtos.Server.newBuilder().setM4(resultS).build());
                    }
                }
                NefitProtos.ProductionS productionS = this.messages.createProductionS(
                    prod.getKey(),prod.getValue(),order.getQuant(),order.getValue()
                );
                Client.writeDelimited(os,NefitProtos.Server.newBuilder().setM6(productionS).build());
            }
            else
            {
                NefitProtos.ProductionS productionS = this.messages.createProductionS(
                    prod.getKey(),prod.getValue(),0,0
                );
                Client.writeDelimited(os,NefitProtos.Server.newBuilder().setM6(productionS).build());
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
