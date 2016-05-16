using System.Collections.Generic;

namespace Model
{
    public class LinkageSection : CobolTreeNode
    {
        public override int StartIndex
        {
            get { return ParentCobolFile.DivisionsAndSection.Linkage.GetValueOrDefault(-1); }
        }

        public override int EndIndex
        {
            get { return ParentCobolFile.DivisionsAndSection.Procedure.GetValueOrDefault(-1); }
        }

        public override List<CobolTreeNode> GetNodes()
        {
            return new List<CobolTreeNode>();
        }

        public LinkageSection(CobolFile cobolFile) : base(cobolFile, "Linkage Section")
        {
        }
    }
}
