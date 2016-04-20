namespace Canal.Utils
{
    using System;
    using System.Linq;

    using Logging;

    using Model.Pictures;

    public class PicParser
    {
        public static readonly PicParser Instance = new PicParser();

        private PicParser()
        {
        }

        /// <summary>
        /// Parses a text containing a PIC-definition: "X(03)" =&gt; "XXX" =&gt; new PicX(3), "S9(2).99" =&gt; "S99.99" =&gt; new PicS9V9(2,2)
        /// </summary>
        /// <param name="textIn">
        /// The text to parse.
        /// </param>
        /// <returns>
        /// A class implementing IPic
        /// </returns>
        public IPic Parse(string textIn)
        {
            var text = textIn.ToUpperInvariant();

            if (string.IsNullOrWhiteSpace(text))
                return new Pic88();

            var picPartResolved = this.ResolveParenthesis(text);

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
                return new PicS9(picPartResolved.Length - 1);

            Logger.Error("Error parsing {0}", text);
            throw new Exception("Error parsing " + text);
        }

        private string ResolveParenthesis(string text)
        {
            var picPartResolved = text;

            while (picPartResolved.Contains("("))
            {
                var start = picPartResolved.IndexOf('(');
                var end = picPartResolved.IndexOf(')', start);
                if (start < 1 || end < start) throw new ArgumentException(@"Incorrect number of parenthesis in text " + text, "text"); // error condition
                var textInParenthesis = picPartResolved.Substring(start + 1, end - start - 1);
                var occurrences = int.Parse(textInParenthesis);
                var type = picPartResolved[start - 1];

                picPartResolved = string.Format("{0}{1}{2}", picPartResolved.Substring(0, start - 1), new string(type, occurrences), picPartResolved.Substring(end + 1));
            }

            return picPartResolved;
        }
    }
}