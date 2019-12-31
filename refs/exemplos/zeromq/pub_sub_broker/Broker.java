package pub_sub_broker;
import org.zeromq.ZMQ;

public class Broker {

  public static void main(String[] args) {
    ZMQ.Context context = ZMQ.context(1);
    ZMQ.Socket pubs = context.socket(ZMQ.XSUB);
    ZMQ.Socket subs = context.socket(ZMQ.XPUB);
    pubs.bind("tcp://*:"+args[0]);
    subs.bind("tcp://*:"+args[1]);
    new Proxy(context, pubs, subs).poll();
  }
}
