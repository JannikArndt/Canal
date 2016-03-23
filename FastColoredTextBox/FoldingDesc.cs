using System.Text.RegularExpressions;

namespace FastColoredTextBoxNS
{
    public class FoldingDesc
    {
        public string startMarkerRegex;
        public string finishMarkerRegex;
        public RegexOptions options = RegexOptions.None;
    }
}