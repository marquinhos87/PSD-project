package nefit.client;

import com.google.protobuf.MessageLite;
import com.google.protobuf.Parser;
import nefit.shared.Util;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;

public class Connection implements AutoCloseable
{
    private final Socket socket;

    private final InputStream in;
    private final OutputStream out;

    public Connection(InetSocketAddress endpoint) throws IOException
    {
        this.socket = new Socket(endpoint.getHostString(), endpoint.getPort());

        this.in = this.socket.getInputStream();
        this.out = this.socket.getOutputStream();
    }

    public < T > T receive(Parser< T > msgParser) throws IOException
    {
        return Util.read(this.in, msgParser);
    }

    public void send(MessageLite msg) throws IOException
    {
        Util.write(this.out, msg);
    }

    @Override
    public void close() throws IOException
    {
        this.socket.close();
    }
}
