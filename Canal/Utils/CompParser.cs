namespace Canal.Utils
{
    using Model.Pictures;

    public class CompParser
    {
        public static readonly CompParser Instance = new CompParser();

        private CompParser()
        {
        }

        public CompType Parse(string text)
        {
            text = text.Trim();
            if (!text.StartsWith("COMP")) return CompType.None;
            if (text.EndsWith("P") || text.EndsWith("L")) return CompType.Comp;
            if (text.EndsWith("1")) return CompType.Comp1;
            if (text.EndsWith("2")) return CompType.Comp2;
            if (text.EndsWith("3")) return CompType.Comp3;
            if (text.EndsWith("4")) return CompType.Comp4;
            return CompType.None;
        }
    }
}