package nefit.arbiter;

import com.google.protobuf.InvalidProtocolBufferException;
import com.google.protobuf.MapEntry;
import javafx.util.Pair;
import nefit.proto.NefitProtos;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Arbiter implements Runnable
{
    private ZMQ.Socket socket;
    private ZContext context;
    private Messages messages;

    //Map<NameManufacturer,List<Pair<Product,Best Order>>>
    private Map<String,List<Pair<NefitProtos.DisponibilityN, NefitProtos.OrderS>>> negotiations;

    //Map<Pair<NameManufacturer,Product>,List<NameImporter>>
    private Map<Pair<String,String>,List<String>> importerNego;

    //Map<NameImporter,List<NameManufacturer>>
    private Map<String,List<String>> subscribers;

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

        //TODO
        while(true)
        {
            try
            {
                byte[] reply = socket.recv(0);
                NefitProtos.Negotiator negotiator = NefitProtos.Negotiator.parseFrom(reply);

                if(negotiator.hasDisponibility())
                {
                    executeDisponibility(negotiator.getDisponibility());
                }

                if(negotiator.hasOrder())
                {
                    executeOrder(negotiator.getOrder());
                }

                if(negotiator.hasGet())
                {
                    executeGet(negotiator.getGet());
                }

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
        if(!this.negotiations.containsKey(disponibility.getNameM()))
        {
            List<Pair<NefitProtos.DisponibilityN, NefitProtos.OrderS>> aux = new ArrayList<>();
            aux.add(new Pair<>(disponibility, null));
            this.negotiations.put(disponibility.getNameM(),aux);
        }
        else
        {
            this.negotiations.get(disponibility.getNameM()).add(new Pair<>(disponibility,null));
        }
        //TODO: make timeouts to send result
    }

    private void executeOrder(NefitProtos.OrderN order) {
        NefitProtos.OrderAckS ack = NefitProtos.OrderAckS.newBuilder().build();
        if(this.negotiations.containsKey(order.getNameM())) {
            for (Pair<NefitProtos.DisponibilityN, NefitProtos.OrderS> aux : this.negotiations.get(order.getNameM())) {
                if(aux.getKey().getNameP().equals(order.getNameP())) {
                    if (aux.getKey().getMaximun() >= order.getQuant() && aux.getKey().getMinimun() <= order.getQuant() && aux.getKey().getValue() <= order.getValue()) {
                        float old_v = aux.getValue().getValue() * aux.getValue().getQuant();
                        float new_v = order.getValue() * order.getQuant();
                        if (new_v > old_v) {
                            //Send ack to old importer
                            this.socket.send(
                                this.messages.createOrderAckS(
                                    false, "Your order to " + order.getNameM() + ":" + order.getNameP() + "is outdated", aux.getValue().getNameI(), true
                                ).toByteArray(), 0);
                            ack = this.messages.createOrderAckS(true, null, order.getNameI(), false);
                        }
                    } else {
                        ack = this.messages.createOrderAckS(false, "Don't have the minimums required or the quantity is very high", order.getNameI(), false);
                    }
                }
            }
            if(ack.hasNameI())
                this.socket.send(ack.toByteArray(),0);
            else
                this.socket.send(this.messages.createOrderAckS(false,"The Manufacturer don't has this product",order.getNameI(),false).toByteArray(),0);
        }
        else
            this.socket.send(this.messages.createOrderAckS(false,"The Manufacturer don't exist",order.getNameI(),false).toByteArray(),0);
    }

    private void executeGet(NefitProtos.GetN get)
    {
        List<NefitProtos.InfoS> res = new ArrayList<>();
        for(Map.Entry<String,List<Pair<NefitProtos.DisponibilityN,NefitProtos.OrderS>>> entry: this.negotiations.entrySet()) {
            for (String str : this.subscribers.get(get.getNameI()))
                if (str.equals(entry.getKey()))
                    for(Pair<NefitProtos.DisponibilityN,NefitProtos.OrderS> aux: entry.getValue())
                        res.add(
                            this.messages.createInfoS(
                                str,
                                aux.getKey().getNameP(),
                                aux.getKey().getMaximun(),
                                aux.getKey().getMinimun(),
                                aux.getKey().getValue(),
                                aux.getKey().getPeriod(),
                                get.getNameI()
                            )
                        );
        }
        this.socket.send(this.messages.createNegotiationsS(get.getNameI(),res).toByteArray(),0);
    }

    private void executeSub(NefitProtos.SubN sub)
    {
        this.subscribers.put(sub.getNameI(),sub.getSubsList().subList(0,sub.getSubsCount()));
    }
}
