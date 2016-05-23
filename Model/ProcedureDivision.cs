using System.Collections.Generic;

namespace Model
{
    public class ProcedureDivision : Division
    {
        public override List<CobolTreeNode> GetNodes()
        {
            return new List<CobolTreeNode>(Sections);
        }

        public List<Section> Sections { get; set; }

        public ProcedureDivision(CobolFile cobolFile) : base(cobolFile, "Procedure Division",
            cobolFile.DivisionsAndSection.Procedure.GetValueOrDefault(-1),
            cobolFile.Text.Length - 1)
        {
            Sections = new List<Section>();
        }
    }
}
