package nefit.client;

import java.util.function.Function;

public class Util
{
    private Util()
    {
    }

    public static void ensure(boolean condition, String errorMessage)
    {
        ensure(condition, errorMessage, IllegalArgumentException::new);
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
