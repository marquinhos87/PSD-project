package nefit.arbiter;

import com.google.protobuf.InvalidProtocolBufferException;
import javafx.util.Pair;
import nefit.proto.NefitProtos;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Arbiter implements Runnable
{
    private ZMQ.Socket socket;
    private ZContext context;
    private Messages messages;
    private Lock l = new ReentrantLock();

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
        this.messages = new Messages();
        this.context = new ZContext();
        this.socket = context.createSocket(SocketType.REQ);
        this.socket.connect("tcp://localhost:12345");

        this.negotiations = new HashMap<>();
        this.subscribers = new HashMap<>();
        this.importerNego = new HashMap<>();
        this.importerManu = new HashMap<>();

        while(true)
        {
            try
            {
                byte[] reply = this.socket.recv(0);
                NefitProtos.Negotiator negotiator = NefitProtos.Negotiator.parseFrom(reply);

                if(negotiator.hasDisponibility())
                {
                    executeDisponibility(negotiator.getDisponibility());
                }

                if(negotiator.hasOrder())
                {
                    executeOrder(negotiator.getOrder());
                }

                /*if(negotiator.hasGet())
                {
                    executeGet(negotiator.getGet());
                }*/

                if(negotiator.hasSub())
                {
                    executeSub(negotiator.getSub());
                }
            }
            catch (InvalidProtocolBufferException e) {
                e.printStackTrace();
            }
        }
    }

    private void executeDisponibility(NefitProtos.DisponibilityN disponibility)
    {
        this.negotiations.put(new Pair<>(disponibility.getNameM(),disponibility.getNameP()),new Pair<>(disponibility, null));
        this.importerNego.put(new Pair<>(disponibility.getNameM(),disponibility.getNameP()),new ArrayList<>());
        this.disp = disponibility;

        //Thread that leads to
        new Thread(this::executeResult).start();

        if(!this.importerManu.containsKey(disponibility.getNameM()))
            this.importerManu.put(disponibility.getNameM(),new ArrayList<>());
        else
            for(String str: this.importerManu.get(disponibility.getNameM()))
                this.socket.send(
                    this.messages.createInfoS(
                        disponibility.getNameM(),
                        disponibility.getNameP(),
                        disponibility.getMaximun(),
                        disponibility.getMinimun(),
                        disponibility.getValue(),
                        disponibility.getPeriod(),
                        str)
                    .toByteArray(),0);
    }

    private void executeOrder(NefitProtos.OrderN order) {
        NefitProtos.OrderAckS ack;
        if(this.negotiations.containsKey(new Pair<>(order.getNameM(),order.getNameP())))
        {
            Pair<NefitProtos.DisponibilityN,NefitProtos.OrderN> aux = this.negotiations.get(new Pair<>(order.getNameM(),order.getNameP()));
            if (aux.getKey().getMaximun() >= order.getQuant() && aux.getKey().getMinimun() <= order.getQuant() && aux.getKey().getValue() <= order.getValue())
            {
                float old_v = aux.getValue().getValue() * aux.getValue().getQuant();
                float new_v = order.getValue() * order.getQuant();
                if (new_v > old_v) {
                    //Send ack to old importer
                    this.socket.send(
                        this.messages.createOrderAckS(
                            false, "Your order to " + order.getNameM() + ":" + order.getNameP() + "is outdated", aux.getValue().getNameI(), true
                        ).toByteArray(), 0);
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
            this.socket.send(ack.toByteArray(),0);
        }
    }

    private void executeSub(NefitProtos.SubN sub)
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

    private void executeResult()
    {
        NefitProtos.DisponibilityN disp = NefitProtos.DisponibilityN.newBuilder(this.disp).build();
        try {
            Thread.sleep(disp.getPeriod()*1000L);
            this.l.lock();
            Pair<String,String> prod = new Pair<>(disp.getNameM(),disp.getNameP());
            if(!(this.negotiations.get(prod).getValue() == null))
            {
                NefitProtos.OrderN order = this.negotiations.get(prod).getValue();
                this.socket.send(this.messages.createResultS(true,order.getNameM() + ":" + order.getNameP(),order.getNameI()).toByteArray(),0);
                for(String str: this.importerNego.get(prod))
                    if(!str.equals(order.getNameI()))
                        this.socket.send(this.messages.createResultS(false,order.getNameM() + ":" + order.getNameP(),str).toByteArray(),0);
                this.socket.send(
                    this.messages.createProductionS(
                        prod.getKey(),prod.getValue(),order.getQuant(),order.getValue()
                    ).toByteArray(),0
                );
            }
            else
            {
                this.socket.send(
                    this.messages.createProductionS(
                        prod.getKey(),prod.getValue(),0,0
                    ).toByteArray(),0
                );
            }
            this.negotiations.remove(prod);
            this.importerNego.remove(prod);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        finally {
            this.l.unlock();
        }
    }
}
