package nefit.proto;

import com.google.protobuf.MessageLite;
import com.google.protobuf.Parser;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.ByteBuffer;

public class ProtoUtil
{
    private ProtoUtil()
    {
    }

    public static < T > T read(InputStream in, Parser< T > msgParser)
        throws IOException
    {
        final var header = in.readNBytes(4);
        assert header.length == 4;

        final var msgSize = ByteBuffer.wrap(header).getInt();

        final var bytes = in.readNBytes(msgSize);
        assert bytes.length == msgSize;

        return msgParser.parseFrom(bytes);
    }

    public static void write(OutputStream out, MessageLite msg)
        throws IOException
    {
        final var msgSize = msg.getSerializedSize();
        final var header = ByteBuffer.allocate(4).putInt(msgSize).array();

        out.write(header);
        msg.writeTo(out);
    }
}
