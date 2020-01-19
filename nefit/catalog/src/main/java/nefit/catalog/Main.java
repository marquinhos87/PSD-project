package nefit.catalog;

public class Main
{
    public static void main(String[] args) throws Exception
    {
        try (final var app = new CatalogApplication())
        {
            app.run(new String[] { "server" });
        }
    }
}
