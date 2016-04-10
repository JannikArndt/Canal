using Logging;
using System;
using System.Linq;
// ReSharper disable MemberCanBePrivate.Global
// ReSharper disable AutoPropertyCanBeMadeGetOnly.Global
// ReSharper disable MemberCanBeProtected.Global
// ReSharper disable UnusedAutoPropertyAccessor.Global

namespace Model.Pictures
{
    public interface IPic
    {
        int Length { get; set; }

        string Value { get; set; }

        CompType CompType { get; set; }
    }

    public static class Pic
    {
        /// <summary>
        /// Parses a text containing a PIC-definition: "X(03)" => "XXX" => new PicX(3), "S9(2).99" => "S99.99" => new PicS9V9(2,2)
        /// </summary>
        public static IPic Parse(string text)
        {
            var picPartResolved = ResolveParenthesis(text);

            var indexOfComp = picPartResolved.IndexOf("COMP", StringComparison.Ordinal);
            if (indexOfComp > 0)
            {
                picPartResolved = picPartResolved.Substring(0, indexOfComp).TrimEnd();
            }

            // PIC XXX
            if (picPartResolved.All(c => c == 'X'))
                return new PicX(picPartResolved.Length);

            // contains V?
            var decimalPointIndex = picPartResolved.IndexOf('V');
            if (decimalPointIndex > -1)
            {
                // PIC S99V99
                if (picPartResolved[0] == 'S')
                    return new PicS9V9(decimalPointIndex - 1, picPartResolved.Length - decimalPointIndex - 1);

                // PIC 9V99
                return new Pic9V9(decimalPointIndex, picPartResolved.Length - decimalPointIndex - 1);
            }

            // PIC 99
            if (picPartResolved.All(c => c == '9'))
                return new Pic9(picPartResolved.Length);

            // PIC S99
            if (picPartResolved[0] == 'S' && picPartResolved.Skip(1).All(c => c == '9'))
                return new PicS9(picPartResolved.Length);

            Logger.Singleton.AddMsg(2, "Error parsing {0}", text);
            throw new Exception("Error parsing " + text);
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


    }

    public class PicX : IPic
    {
        private string _value;

        public int Length { get; set; }

        public string Value
        {
            get { return _value; }
            set { _value = value.StartsWith("SPACE") ? new string(' ', Length) : value.Trim('\"', '"', '\\'); }
        }

        public CompType CompType { get; set; }

        public PicX(int length)
        {
            Length = length;
        }

        public override string ToString()
        {
            return string.Format("PIC X{0}", Length > 1 ? "(" + Length + ")" : string.Empty);
        }
    }

    public class Pic9 : IPic
    {
        protected int? _value;

        public int Length { get; set; }

        public CompType CompType { get; set; }

        public Pic9(int length, CompType comp = CompType.None)
        {
            Length = length;
            CompType = comp;
        }

        public string Value
        {
            get { return _value != null ? _value.ToString() : null; }
            set { _value = string.IsNullOrWhiteSpace(value) ? (int?)null : value.StartsWith("ZERO") ? 0 : int.Parse(value); }
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

        public new string Value
        {
            get { return base.Value; }
            set { base.Value = value.Substring(1); }
        }

        public override string ToString()
        {
            return string.Format("PIC S9{0}", Length > 1 ? "(" + Length + ")" : string.Empty);
        }
    }

    public class Pic9V9 : Pic9
    {
        private new decimal? _value;

        public int IntegersLength { get; set; }

        public int FractionsLength { get; set; }

        public new string Value
        {
            get { return _value != null ? _value.ToString() : null; }
            set { _value = string.IsNullOrWhiteSpace(value) ? (decimal?)null : value.StartsWith("ZERO") ? 0 : decimal.Parse(value); }
        }

        public Pic9V9(int integersLength, int fractionsLength, CompType comp = CompType.None) : base(integersLength + fractionsLength, comp)
        {
            IntegersLength = integersLength;
            FractionsLength = fractionsLength;
        }

        public override string ToString()
        {
            return string.Format("PIC 9{0}V9{1}", IntegersLength > 1 ? "(" + IntegersLength + ")" : string.Empty, FractionsLength > 1 ? "(" + FractionsLength + ")" : string.Empty);
        }
    }

    public class PicS9V9 : Pic9V9
    {
        public PicS9V9(int integersLength, int fractionsLength, CompType comp = CompType.None) : base(integersLength, fractionsLength, comp)
        {
        }

        public override string ToString()
        {
            return string.Format("PIC S9{0}V9{1}", IntegersLength > 1 ? "(" + IntegersLength + ")" : string.Empty, FractionsLength > 1 ? "(" + FractionsLength + ")" : string.Empty);
        }
    }

    public class Pic88 : IPic
    {
        public int Length { get; set; }
        public string Value { get; set; }
        public CompType CompType { get; set; }

        public override string ToString()
        {
            return string.Format("VALUE {0}", Value);
        }
    }
}