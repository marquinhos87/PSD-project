package nefit.client;

import javafx.util.Pair;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.BufferedReader;

public class Importer implements Runnable {
    private String name;
    private String pass;
    private BufferedReader in;
    private PrintWriter out;

    public Importer(Pair<String,String> auth, BufferedReader in, PrintWriter out)
    {
        this.name = auth.getKey();
        this.pass = auth.getValue();
        this.in = in;
        this.out = out;
    }

    @Override
    public void run()
    {
        Messages messages = new Messages();
        //Authentication
        while(true) {
            try {
                out.println("Login(l) or Register and Login(r)");
                out.flush();
                String auth = in.readLine();
                if(auth.equals("l")) {
                    //TODO : MsgAuth Login

                    //Replace true by MsgAck.getAck();
                    if(true) break;
                    else
                    {
                        out.println("Invalid data");
                        out.flush();
                    }
                }
                else {
                    //TODO : MsgAuth Register

                    //Replace true by MsgAck.getAck();
                    if(true)
                    {
                        //TODO : MsgAuth Login

                        //Replace true by MsgAck.getAck();
                        if(true) break;
                    }
                    else
                    {
                        out.println("Importer yet registered");
                        out.flush();
                    }
                }

            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        //Orders
        //TODO
    }
}
