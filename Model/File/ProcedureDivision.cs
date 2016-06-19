using System.Collections.Generic;

namespace Model.File
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
            cobolFile.DivisionsAndSection.Procedure.HasValue && cobolFile.DivisionsAndSection.Procedure.Value > 0 ? cobolFile.Text.Length - 1 : 0)
        {
            Sections = new List<Section>();
        }
    }
}
