using FastColoredTextBoxNS;
using System;
using System.Linq;
using System.Text.RegularExpressions;
using System.Windows.Forms;

namespace Canal
{
    public partial class CodeBox : FastColoredTextBox
    {
        public CobolFile CobolFile { get; set; }

        public CodeBox()
        {
            InitializeComponent();
        }

        public void SetFile(CobolFile file)
        {
            CobolFile = file;

            Text = file.Text;
            Language = Language.Cobol;
            HighlightingRangeType = HighlightingRangeType.VisibleRange;
            SyntaxHighlighter.HighlightSyntax(Language.Cobol, Range);
        }

        public void FindNext(string pattern, bool matchCase, bool regex, bool wholeWord, bool firstSearch = false)
        {
            try
            {
                RegexOptions opt = matchCase ? RegexOptions.None : RegexOptions.IgnoreCase;
                if (!regex)
                    pattern = Regex.Escape(pattern);
                if (wholeWord)
                    pattern = "\\b" + pattern + "\\b";
                //
                Range searchRange = firstSearch ? new Range(this, Range.Start, Range.End) : new Range(this, Selection.End, Range.End);

                var results = searchRange.GetRangesByLines(pattern, opt | RegexOptions.Compiled).ToList();
                foreach (var r in results)
                {
                    Selection = r;
                    DoSelectionVisible();
                    Invalidate();
                    return;
                }

                if (!results.Any() && !firstSearch)
                    FindNext(pattern, matchCase, regex, wholeWord, true);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message);
            }
        }
    }
}
