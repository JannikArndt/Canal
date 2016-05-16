using System.Collections.Generic;

namespace Model
{
    public class LinkageSection : CobolTreeNode
    {
        protected override int StartIndex
        {
            get { return ParentCobolFile.DivisionsAndSection.Linkage.GetValueOrDefault(-1); }
        }

        protected override int EndIndex
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
