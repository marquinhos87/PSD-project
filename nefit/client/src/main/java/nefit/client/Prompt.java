package nefit.client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;

public class Prompt implements AutoCloseable {

    private final BufferedReader in;
    private final PrintWriter out;

    public Prompt() throws IOException {
        this.in = new BufferedReader(new InputStreamReader(System.in));

        try {
            this.out = new PrintWriter(System.out);
        }
        catch (Throwable t)
        {
            this.in.close();
            throw t;
        }
    }

    public String input(String prompt) throws IOException {
        this.out.print(prompt);
        this.out.flush();

        return this.in.readLine();
    }

    public void print(String line)
    {
        this.out.println(line);
        this.out.flush();
    }

    public void printSuccess(String error)
    {
        this.out.format("\033[32m%s\033[0m\n", error);
        this.out.flush();
    }

    public void printNotice(String error)
    {
        this.out.format("\033[33m%s\033[0m\n", error);
        this.out.flush();
    }

    public void printError(String error)
    {
        this.out.format("\033[31m%s\033[0m\n", error);
        this.out.flush();
    }

    public void fail(String error)
    {
        this.printError(error);
        System.exit(1);
    }

    @Override
    public void close() throws IOException {
        this.in.close();
        this.out.close();
    }
}
