package nefit.shared;

import com.google.protobuf.MessageLite;
import com.google.protobuf.Parser;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.util.function.Function;
import java.util.function.Supplier;

public class Util
{
    private Util()
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

    public static void ensure(boolean condition)
    {
        ensure(condition, IllegalArgumentException::new);
    }

    public static void ensure(boolean condition, String errorMessage)
    {
        ensure(condition, errorMessage, IllegalArgumentException::new);
    }

    public static < T extends Throwable > void ensure(
        boolean condition,
        Supplier< T > throwableConstructor
    ) throws T
    {
        if (!condition)
            throw throwableConstructor.get();
    }

    public static < T extends Throwable > void ensure(
        boolean condition,
        String errorMessage,
        Function< String, T > throwableConstructor
    ) throws T
    {
        if (!condition)
            throw throwableConstructor.apply(errorMessage);
    }
}
