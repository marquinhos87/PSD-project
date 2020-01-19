package nefit.client;

import com.google.protobuf.MessageLite;
import com.google.protobuf.Parser;
import nefit.proto.ProtoUtil;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;

public class Connection implements AutoCloseable {
    private final Socket socket;

    private final InputStream in;
    private final OutputStream out;

    public Connection(String host, int port) throws IOException {
        this.socket = new Socket(host, port);

        this.in = this.socket.getInputStream();
        this.out = this.socket.getOutputStream();
    }

    public <T> T receive(Parser<T> msgParser) throws IOException {
        return ProtoUtil.read(this.in, msgParser);
    }

    public void send(MessageLite msg) throws IOException {
        ProtoUtil.write(this.out, msg);
    }

    @Override
    public void close() throws IOException {
        this.socket.close();
    }
}
