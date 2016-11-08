using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VariableUsageAnalyzer
{
    class LineDto
    {
        public LineDto(string text, int number)
        {
            this.Text = text;
            this.Number = number;
        }

        public String Text { get; set; }
        public int Number { get; set; }
    }
}
