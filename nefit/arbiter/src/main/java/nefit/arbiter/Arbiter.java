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

public class Arbiter implements Runnable
{
    private ZMQ.Socket socket;
    private ZContext context;
    private Messages messages;
    private List<Pair<NefitProtos.DisponibilityN, List<NefitProtos.OrderS>>> negotiations;
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

        this.negotiations = new ArrayList<>();
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
        this.negotiations.add(new Pair<>(disponibility,new ArrayList<>()));

    }

    private void executeOrder(NefitProtos.OrderN order)
    {
        //TODO
        //Como enviar pelo socket sendo o 'request' a nossa mensagem
        //socket.send(request.getBytes(ZMQ.CHARSET), 0);
    }

    private void executeGet(NefitProtos.GetN get)
    {
        //TODO

    }

    private void executeSub(NefitProtos.SubN sub)
    {
        //TODO
        this.subscribers.put(sub.get)
    }
}
