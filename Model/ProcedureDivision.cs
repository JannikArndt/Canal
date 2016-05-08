using System.Collections.Generic;

namespace Model
{
    public class ProcedureDivision : Division
    {
        protected override int StartIndex
        {
            get { return ParentCobolFile.DivisionsAndSection.Procedure.GetValueOrDefault(-1); }
        }

        protected override int EndIndex
        {
            get { return ParentCobolFile.Text.Length - 1; }
        }

        public List<Section> Sections { get; private set; }

        public ProcedureDivision(CobolFile cobolFile) : base(cobolFile, "Procedure Division")
        {
            Sections = new List<Section>();
        }
    }
}
