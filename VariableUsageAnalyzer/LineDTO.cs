using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Model.File;

namespace VariableUsageAnalyzer
{
    class LineDto
    {
        public LineDto(string text, int number, Variable foundVariable, CobolFile containingFile)
        {
            Text = text;
            Number = number;
            FoundVariable = foundVariable;
            ContainingFile = containingFile;
        }

        public String Text { get; set; }
        public int Number { get; set; }
        public Variable FoundVariable { get; set; }
        public CobolFile ContainingFile { get; set; }
    }
}
