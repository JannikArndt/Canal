using FastColoredTextBoxNS;

namespace Canal
{
    public partial class CodeBox : FastColoredTextBox
    {
        public CobolFile CobolFile { get; set; }

        public CodeBox()
        {
            InitializeComponent();
        }

        public CodeBox(CobolFile file)
        {
            this.CobolFile = file;

            Text = file.Text;
            Language = Language.Cobol;
            HighlightingRangeType = HighlightingRangeType.VisibleRange;

            SyntaxHighlighter.HighlightSyntax(Language.Cobol, Range);
        }
    }
}
