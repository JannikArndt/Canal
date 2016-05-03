using System.Collections.Generic;

namespace Model
{
    public class ProcedureDivision : Division
    {
        public List<Section> Sections { get; private set; }

        public ProcedureDivision(string sourceCode, int indexProcedureDivision)
            : base(sourceCode, "Procedure Division", indexProcedureDivision)
        {
            Sections = new List<Section>();
        }
    }
}
