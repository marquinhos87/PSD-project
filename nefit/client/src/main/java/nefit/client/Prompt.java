package nefit.client;

import java.io.PrintWriter;

public class Prompt {
    private PrintWriter out;

    public Prompt(PrintWriter out)
    {
        this.out = out;
    }

    public void printMessages(String text)
    {
        this.out.format("\033[32m%s\033[0m\n", text);
        this.out.flush();
    }

    public void printOthers(String text)
    {
        this.out.format("\033[34m%s\033[0m\n", text);
        this.out.flush();
    }

    public void printWarning(String text)
    {
        this.out.format("\033[33m%s\033[0m\n", text);
        this.out.flush();
    }

    public void printError(String text)
    {
        this.out.format("\033[31m%s\033[0m\n", text);
        this.out.flush();
    }
}
