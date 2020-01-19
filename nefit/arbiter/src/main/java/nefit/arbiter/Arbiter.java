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
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Arbiter implements Runnable
{
    private ZMQ.Socket socketZ;
    private ZContext context;
    private Connection connection;
    private Prompt prompt;

    private NefitProto.ServerToArbiterAnnounce disp;

    /**
     * Map where the Key is a Pair with the name of the Manufacturer and name of the Product
     * The Value for this Key is a Pair with the information of the product and the best order to the product
     * The best order to the product can be 'null' if doesn't exist a order that satisfies the requirements of the product
     * Map<NameManufacturer,Pair<Product,Best Order>>
     */
    private Map<String,Pair< NefitProto.ServerToArbiterAnnounce, NefitProto.ServerToArbiterOffer>> negotiations;

    /**
     * Map where the Key is a Pair with the name of the Manufacturer and name of the Product
     * The Value for this Key is the list of all importers that make at least one offer to the product
     * Map<Pair<NameManufacturer,Product>,List<NameImporter>>
     */
    private Map<String,List<String>> importerNego;

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

                //if(negotiator.hasOffer())
                  //  executeOrder(negotiator.getOffer());
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
                this.socketZ.recv(0);

                byte[] reply = this.socketZ.recv(0);

                //String[] aux = new String(reply).split("!",2);

                //byte[] a = Arrays.copyOfRange(reply,aux[0].length(),reply.length-aux[0].length());

                System.out.println(reply);

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
        this.negotiations.put(disponibility.getManufacturerName()+disponibility.getProductName(),new Pair<>(disponibility, null));
        this.importerNego.put(disponibility.getManufacturerName()+disponibility.getProductName(),new ArrayList<>());
        this.disp = disponibility;

        //Thread that leads to
        new Thread(this::executeResult).start();

        this.socketZ.subscribe(disponibility.getManufacturerName()+disponibility.getProductName());

        if(!this.importerManu.containsKey(disponibility.getManufacturerName()))
            this.importerManu.put(disponibility.getManufacturerName(),new ArrayList<>());
        else
        {
            for (String str : this.importerManu
                .get(disponibility.getManufacturerName()))
            {
                NefitProto.ArbiterToServerNewProduct infoS = NefitProto.ArbiterToServerNewProduct
                    .newBuilder()
                    .setManufacturerName(disponibility.getManufacturerName())
                    .setProductName(disponibility.getProductName())
                    .setMaxQuantity(disponibility.getMaxQuantity())
                    .setMinQuantity(disponibility.getMinQuantity())
                    .setMinUnitPrice(disponibility.getMinUnitPrice())
                    .setTimeout(disponibility.getTimout())
                    .setImporterName(str).build();
                connection.send(
                    NefitProto.ArbiterToServer.newBuilder().setProduct(infoS)
                        .build());
            }
        }
        NefitProto.ArbiterToServerAnnounceAccepted accepted = NefitProto.ArbiterToServerAnnounceAccepted.newBuilder()
            .setManufacturerName(disponibility.getManufacturerName())
            .setProductName(disponibility.getProductName()).build();
        connection.send(NefitProto.ArbiterToServer.newBuilder().setAccepted(accepted).build());
    }

    private synchronized void executeOrder(NefitProto.ServerToArbiterOffer order) throws IOException {
        if(this.negotiations.containsKey(order.getManufacturerName()+order.getProductName()))
        {
            Pair< NefitProto.ServerToArbiterAnnounce, NefitProto.ServerToArbiterOffer> aux = this.negotiations.get(order.getManufacturerName()+order.getProductName());
            if (aux.getKey().getMaxQuantity() >= order.getQuantity() && aux.getKey().getMinQuantity() <= order.getQuantity() && aux.getKey().getMinUnitPrice() <= order.getUnitPrice())
            {
                if(aux.getValue() == null){
                    //Send ack to new importer
                    NefitProto.ArbiterToServerOfferSubmitted ack = NefitProto.ArbiterToServerOfferSubmitted
                        .newBuilder()
                        .setManufacturerName(order.getManufacturerName())
                        .setProductName(order.getProductName())
                        .setImporterName(order.getImporterName())
                        .build();
                    connection.send(NefitProto.ArbiterToServer.newBuilder()
                        .setSubmitted(ack).build());
                    //Replace Order
                    this.negotiations.put(
                            order.getManufacturerName()+
                            order.getProductName(),
                        new Pair<>(aux.getKey(), order)
                    );
                }
                else
                {
                    float old_v = aux.getValue().getUnitPrice() * aux.getValue()
                        .getQuantity();
                    float new_v = order.getUnitPrice() * order.getQuantity();
                    if (new_v > old_v)
                    {
                        //Send ack to old importer
                        NefitProto.ArbiterToServerOfferOutdated orderAckS = NefitProto.ArbiterToServerOfferOutdated
                            .newBuilder()
                            .setImporterName(aux.getValue().getImporterName())
                            .setManufacturerName(order.getManufacturerName())
                            .setProductName(order.getProductName()).build();
                        connection.send(NefitProto.ArbiterToServer.newBuilder()
                            .setOfferOutdated(orderAckS).build());
                        //Send ack to new importer
                        NefitProto.ArbiterToServerOfferSubmitted ack = NefitProto.ArbiterToServerOfferSubmitted
                            .newBuilder()
                            .setManufacturerName(order.getManufacturerName())
                            .setProductName(order.getProductName())
                            .setImporterName(order.getImporterName())
                            .build();
                        connection.send(NefitProto.ArbiterToServer.newBuilder()
                            .setSubmitted(ack).build());
                        //Replace Order
                        this.negotiations.put(
                                order.getManufacturerName()+
                                order.getProductName(),
                            new Pair<>(aux.getKey(), order)
                        );
                    }
                    else
                    {
                        NefitProto.ArbiterToServerOfferInvalid ack = NefitProto.ArbiterToServerOfferInvalid
                            .newBuilder()
                            .setImporterName(order.getImporterName())
                            .setErrorMessage("Exists a better order")
                            .build();
                        connection.send(NefitProto.ArbiterToServer.newBuilder()
                            .setOfferInvalid(ack).build());
                    }
                }
            } else {
                NefitProto.ArbiterToServerOfferInvalid ack = NefitProto.ArbiterToServerOfferInvalid.newBuilder()
                    .setImporterName(order.getImporterName())
                    .setErrorMessage("Don't have the minimums required or the quantity is very high")
                    .build();
                connection.send(NefitProto.ArbiterToServer.newBuilder().setOfferInvalid(ack).build());
            }
            if(!this.importerNego.get(order.getManufacturerName()+order.getProductName()).contains(order.getImporterName()))
                this.importerNego.get(order.getManufacturerName()+order.getProductName()).add(order.getImporterName());
        }
    }

    private synchronized void executeSub(NefitProto.ServerToArbiterSubscribe sub)
    {
        this.subscribers.put(sub.getImporterName(),sub.getManufacturerNamesList().subList(0,sub.getManufacturerNamesCount()));
        for(String str: sub.getManufacturerNamesList().subList(0,sub.getManufacturerNamesCount())) {
            if (this.importerManu.containsKey(str)) {
                if (!this.importerManu.get(str).contains(sub.getImporterName()))
                    this.importerManu.get(str).add(sub.getImporterName());
            }
            else {
                List<String> aux = new ArrayList<>();
                aux.add(sub.getImporterName());
                this.importerManu.put(str,aux);
            }
        }
    }

    private synchronized void executeResult() {
        NefitProto.ServerToArbiterAnnounce disp = NefitProto.ServerToArbiterAnnounce.newBuilder(this.disp).build();
        try {
            String nameM = disp.getManufacturerName();
            String nameP = disp.getProductName();
            String prod = disp.getManufacturerName()+disp.getProductName();
            wait(disp.getTimout()*1000L);
            if(!(this.negotiations.get(prod).getValue() == null))
            {
                NefitProto.ServerToArbiterOffer order = this.negotiations.get(prod).getValue();
                NefitProto.ArbiterToServerOfferWon resultS = NefitProto.ArbiterToServerOfferWon.newBuilder()
                    .setImporterName(order.getImporterName())
                    .setManufacturerName(order.getManufacturerName())
                    .setQuantity(order.getQuantity())
                    .setProductName(order.getProductName())
                    .setUnitPrice(order.getUnitPrice())
                    .build();
                connection.send(NefitProto.ArbiterToServer.newBuilder().setWon(resultS).build());
                for(String str: this.importerNego.get(prod)) {
                    if (!str.equals(order.getImporterName())) {
                        NefitProto.ArbiterToServerOfferLose result = NefitProto.ArbiterToServerOfferLose.newBuilder()
                            .setImporterName(str)
                            .setManufacturerName(order.getManufacturerName())
                            .setProductName(order.getProductName())
                            .build();
                        connection.send(NefitProto.ArbiterToServer.newBuilder().setLose(result).build());
                    }
                }
                NefitProto.ArbiterToServerAnnounceSold productionS = NefitProto.ArbiterToServerAnnounceSold.newBuilder()
                    .setProductName(order.getProductName())
                    .setManufacturerName(order.getManufacturerName())
                    .setQuantity(order.getQuantity())
                    .setUnitPrice(order.getUnitPrice())
                    .build();
                connection.send(NefitProto.ArbiterToServer.newBuilder().setSold(productionS).build());
            }
            else {
                NefitProto.ArbiterToServerAnnounceNoOffers production = NefitProto.ArbiterToServerAnnounceNoOffers.newBuilder()
                    .setManufacturerName(nameM)
                    .setProductName(nameP)
                    .build();
                connection.send(NefitProto.ArbiterToServer.newBuilder().setNoOffers(production).build());
            }
            this.socketZ.unsubscribe(nameM+nameP);
            this.negotiations.remove(prod);
            this.importerNego.remove(prod);
        } catch (InterruptedException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
