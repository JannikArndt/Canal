using System.Collections.Generic;

namespace Model
{
    public class IdentificationDivision : Division
    {
        // AUTHOR, INSTALLATION, DATE-WRITTEN; DATE-COMPILED; SECURITY

        public override List<CobolTreeNode> GetNodes()
        {
            return new List<CobolTreeNode>();
        }

        public IdentificationDivision(CobolFile cobolFile) : base(cobolFile, "Identification Division",
            cobolFile.DivisionsAndSection.Identification.GetValueOrDefault(-1),
            cobolFile.DivisionsAndSection.Environment.GetValueOrDefault(-1))
        {
        }
    }
}
