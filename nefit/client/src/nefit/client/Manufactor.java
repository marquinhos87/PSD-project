package nefit.client;

import javafx.util.Pair;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;

public class Manufactor implements Runnable
{
    //List<Article> articles;
    private String name;
    private String pass;
    private BufferedReader in;
    private PrintWriter out;

    public Manufactor(Pair<String,String> auth, BufferedReader in, PrintWriter out)
    {
        this.name = auth.getKey();
        this.pass = auth.getValue();
        this.in = in;
        this.out = out;
    }

    @Override
    public void run()
    {
        //Authentication
        while(true)
        {
            try
            {
                out.println("Login(l) or Register and Login(r)");
                out.flush();
                String auth = in.readLine();
                if(auth.equals("l"))
                {
                    //TODO : MsgAuth Login
                    //MsgAuth msgl = messages.createMsgAuth(true,true,this.name,this.pass);

                    //Wait for MsgAck

                    //Replace 'true' by MsgAck.getAck();
                    if(true) break;
                    else
                    {
                        out.println("Invalid data");
                        out.flush();
                    }
                }
                else
                {
                    //TODO : MsgAuth Register
                    //MsgAuth msgl = messages.createMsgAuth(false,true,this.name,this.pass);

                    //Wait for MsgAck

                    //Replace 'true' by MsgAck.getAck();
                    if(true)
                    {
                        //TODO : MsgAuth Login
                        //MsgAuth msgl = messages.createMsgAuth(true,true,this.name,this.pass);

                        //Wait for MsgAck

                        //Replace 'true' by MsgAck.getAck();
                        if(true) break;
                    }
                    else
                    {
                        out.println("Manufactor yet registered");
                        out.flush();
                    }
                }

            }
            catch (IOException e)
            {
                e.printStackTrace();
            }
        }

        //Production
        //TODO
        while(true)
        {
            
        }
    }
}
