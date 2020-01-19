package nefit.client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;

public class Prompt implements AutoCloseable
{

    private final BufferedReader in;
    private final PrintWriter out;

    public Prompt() throws IOException
    {
        this.in = new BufferedReader(new InputStreamReader(System.in));

        try
        {
            this.out = new PrintWriter(System.out);
        }
        catch (Throwable t)
        {
            this.in.close();
            throw t;
        }
    }

    public String input(String prompt) throws IOException
    {
        this.out.print(prompt);
        this.out.flush();

        return this.in.readLine();
    }

    public void printNoNewline(String format, Object... args)
    {
        this.out.format(format, args);
        this.out.flush();
    }

    public void print()
    {
        this.out.println();
        this.out.flush();
    }

    public void print(String format, Object... args)
    {
        this.printColored(null, format, args);
    }

    public void printSuccess(String format, Object... args)
    {
        this.printColored("32", format, args);
    }

    public void printNotice(String format, Object... args)
    {
        this.printColored("33", format, args);
    }

    public void printError(String format, Object... args)
    {
        this.printColored("31", format, args);
    }

    private void printColored(String colorCode, String format, Object... args)
    {
        final var coloredFormat =
            colorCode == null
                ? format + "\n"
                : "\033[" + colorCode + "m" + format + "\033[0m\n";

        this.out.format(coloredFormat, args);
        this.out.flush();
    }

    public void fail(String error)
    {
        this.printError(error);
        System.exit(1);
    }

    @Override
    public void close() throws IOException
    {
        this.in.close();
        this.out.close();
    }
}
