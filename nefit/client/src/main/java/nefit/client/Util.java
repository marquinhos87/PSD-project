package nefit.client;

import java.util.function.Function;
import java.util.function.Supplier;

public class Util
{
    private Util()
    {
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
