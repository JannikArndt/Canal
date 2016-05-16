using System.Collections.Generic;

namespace Model
{
    public class IdentificationDivision : Division
    {
        // AUTHOR, INSTALLATION, DATE-WRITTEN; DATE-COMPILED; SECURITY

        public override int StartIndex
        {
            get { return ParentCobolFile.DivisionsAndSection.Identification.GetValueOrDefault(-1); }
        }

        public override int EndIndex
        {
            get { return ParentCobolFile.DivisionsAndSection.Environment.GetValueOrDefault(-1); }
        }

        public override List<CobolTreeNode> GetNodes()
        {
            return new List<CobolTreeNode>();
        }

        public IdentificationDivision(CobolFile cobolFile) : base(cobolFile, "Identification Division")
        {
        }
    }
}
