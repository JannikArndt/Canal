using FastColoredTextBoxNS;

namespace Canal
{
    public partial class CodeBox : FastColoredTextBox
    {
        private CobolFile file;

        public CodeBox() : base()
        {
            InitializeComponent();
        }

        public CodeBox(CobolFile file) : base()
        {
            this.file = file;

            Text = file.Text;
            Language = Language.Cobol;
            HighlightingRangeType = HighlightingRangeType.VisibleRange;

            SyntaxHighlighter.HighlightSyntax(Language.Cobol, Range);
        }
    }
}
