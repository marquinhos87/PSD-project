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

    public static < T > T read(InputStream input, Parser< T > messageParser)
        throws IOException
    {
        // read message size

        final var header = input.readNBytes(4);
        assert header.length == 4;

        // decode message size

        final var msgSize = ByteBuffer.wrap(header).getInt();
        assert msgSize >= 0;

        // read actual message

        final var bytes = input.readNBytes(msgSize);
        assert bytes.length == msgSize;

        // decode actual message

        return messageParser.parseFrom(bytes);
    }

    public static void write(OutputStream output, MessageLite message)
        throws IOException
    {
        // encode message size

        final var msgSize = message.getSerializedSize();
        final var header = ByteBuffer.allocate(4).putInt(msgSize).array();

        // write message size

        output.write(header);

        // encode and write actual message

        message.writeTo(output);
    }
}
