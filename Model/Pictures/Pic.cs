using System;
using System.Linq;

namespace Model.Pictures
{
    public interface IPic
    {
        void ParseValue(string text);
    }

    public static class Pic
    {
        /// <summary>
        /// Parses a text containing a PIC-definition: "X(03)" => "XXX" => new PicX(3), "S9(2).99" => "S99.99" => new PicS9V9(2,2)
        /// </summary>
        public static IPic Parse(string text)
        {
            var picPartResolved = ResolveParenthesis(text);

            CompType comp = CompType.None;
            var indexOfComp = picPartResolved.IndexOf("COMP", StringComparison.Ordinal);
            if (indexOfComp > 0)
            {
                comp = ParseComp(picPartResolved.Substring(indexOfComp));
                picPartResolved = picPartResolved.Substring(0, indexOfComp).TrimEnd();
            }

            if (picPartResolved.All(c => c == 'X'))
                return new PicX(picPartResolved.Length);

            if (picPartResolved.All(c => c == '9'))
                return new Pic9(picPartResolved.Length, comp);

            if (picPartResolved[0] == 'S' && picPartResolved.Skip(1).All(c => c == '9'))
                return new PicS9(picPartResolved.Length, comp);

            var decimalPointIndex = picPartResolved.IndexOf('V');
            if (picPartResolved[0] == 'S' && decimalPointIndex > 0)
                return new PicS9V9(decimalPointIndex - 1, picPartResolved.Length - decimalPointIndex - 1, comp);

            return null;
        }

        private static string ResolveParenthesis(string text)
        {
            var picPartResolved = text;

            while (picPartResolved.Contains("("))
            {
                var start = picPartResolved.IndexOf('(');
                var end = picPartResolved.IndexOf(')', start);
                if (start < 1 || end < start) throw new ArgumentException("Incorrect number of parenthesis in text " + text, "text"); // error condition
                var textInParenthesis = picPartResolved.Substring(start + 1, end - start - 1);
                var occurrences = int.Parse(textInParenthesis);
                var type = picPartResolved[start - 1];

                picPartResolved = string.Format("{0}{1}{2}", picPartResolved.Substring(0, start - 1), new string(type, occurrences), picPartResolved.Substring(end + 1));
            }

            return picPartResolved;
        }

        private static CompType ParseComp(string text)
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

    public class PicX : IPic
    {
        public int Length { get; set; }

        public string Value { get; set; }

        public PicX(int length)
        {
            Length = length;
        }

        public void ParseValue(string text)
        {
            Value = text.StartsWith("SPACE") ? new string(' ', Length) : text.Trim('\"', '"', '\\');
        }

        public override string ToString()
        {
            return string.Format("PIC X{0}", Length > 1 ? "(" + Length + ")" : string.Empty);
        }
    }

    public class Pic9 : IPic
    {
        public int Length { get; set; }

        public int Value { get; set; }

        public CompType CompType { get; set; }

        public Pic9(int length, CompType comp = CompType.None)
        {
            Length = length;
            CompType = comp;
        }

        public void ParseValue(string text)
        {
            Value = text.StartsWith("ZERO") ? 0 : int.Parse(text);
        }

        public override string ToString()
        {
            return string.Format("PIC 9{0}", Length > 1 ? "(" + Length + ")" : string.Empty);
        }
    }

    public class PicS9 : Pic9
    {
        public PicS9(int length, CompType comp = CompType.None) : base(length, comp)
        {
        }

        public new void ParseValue(string text)
        {
            Value = text.StartsWith("ZERO") ? 0 : int.Parse(text.Substring(1));
        }

        public override string ToString()
        {
            return string.Format("PIC S9{0}", Length > 1 ? "(" + Length + ")" : string.Empty);
        }
    }

    public class PicS9V9 : Pic9
    {
        public int IntegersLength { get; set; }

        public int FractionsLength { get; set; }

        public new decimal Value { get; set; }

        public PicS9V9(int integersLength, int fractionsLength, CompType comp = CompType.None) : base(integersLength + fractionsLength, comp)
        {
            IntegersLength = integersLength;
            FractionsLength = fractionsLength;
        }

        public new void ParseValue(string text)
        {
            Value = text.StartsWith("ZERO") ? 0 : decimal.Parse(text);
        }

        public override string ToString()
        {
            return string.Format("PIC S9{0}V9{1}", IntegersLength > 1 ? "(" + IntegersLength + ")" : string.Empty, FractionsLength > 1 ? "(" + FractionsLength + ")" : string.Empty);
        }
    }

    public class Pic88 : IPic
    {
        public string Value { get; set; }

        public void ParseValue(string text)
        {
            Value = text;
        }

        public override string ToString()
        {
            return string.Format("VALUE {0}", Value);
        }
    }
}