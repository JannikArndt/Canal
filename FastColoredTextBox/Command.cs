namespace FastColoredTextBoxNS
{
    public abstract class Command
    {
        public TextSource ts;
        public abstract void Execute();
    }
}