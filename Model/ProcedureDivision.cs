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

        public override List<CobolTreeNode> GetNodes()
        {
            return new List<CobolTreeNode>(Sections);
        }

        public List<Section> Sections { get; }

        public ProcedureDivision(CobolFile cobolFile) : base(cobolFile, "Procedure Division")
        {
            Sections = new List<Section>();
        }
    }
}
