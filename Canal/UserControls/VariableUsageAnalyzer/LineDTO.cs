using Model.File;

namespace Canal.UserControls.VariableUsageAnalyzer
{
    internal class LineDto
    {
        public string Text { get; private set; }

        public int Number { get; private set; }

        public Variable FoundVariable { get; private set; }

        public CobolFile ContainingFile { get; private set; }

        public LineDto(string text, int number, Variable foundVariable, CobolFile containingFile)
        {
            Text = text;
            Number = number;
            FoundVariable = foundVariable;
            ContainingFile = containingFile;
        }
    }
}
