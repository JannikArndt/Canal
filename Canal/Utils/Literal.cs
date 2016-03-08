namespace Canal.Utils
{
    public class Literal
    {
        public string Name { get; set; }

        public UsedAs UsedAs { get; set; }

        public Literal(string name, UsedAs usedAs)
        {
            Name = name;
            UsedAs = usedAs;
        }
    }

    public enum UsedAs
    {
        Unknown,
        Input,
        Output,
        Both
    }
}