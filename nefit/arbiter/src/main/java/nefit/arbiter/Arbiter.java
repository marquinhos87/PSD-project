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
    private Map<String,List<Pair<NefitProtos.DisponibilityN, List<NefitProtos.OrderS>>>> negotiations;
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
            List<Pair<NefitProtos.DisponibilityN, List<NefitProtos.OrderS>>> aux = new ArrayList<>();
            aux.add(new Pair<>(disponibility, new ArrayList<>()));
            this.negotiations.put(disponibility.getNameM(),aux);
        }
        else
        {
            this.negotiations.get(disponibility.getNameM()).add(new Pair<>(disponibility,new ArrayList<>()));
        }
        //TODO: make timeouts to send result
    }

    private void executeOrder(NefitProtos.OrderN order)
    {
        
    }

    private void executeGet(NefitProtos.GetN get)
    {
        List<String> subs = this.subscribers.get(get.getNameI());
        List<NefitProtos.InfoS> res = new ArrayList<>();
        for(Map.Entry<String,List<Pair<NefitProtos.DisponibilityN,List<NefitProtos.OrderS>>>> entry: this.negotiations.entrySet()) {
            for (String str : subs)
                if (str.equals(entry.getKey()))
                    for(Pair<NefitProtos.DisponibilityN,List<NefitProtos.OrderS>> aux: entry.getValue())
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
