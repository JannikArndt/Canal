using System.Collections.Generic;

namespace Model
{
    public class WorkingStorageSection : CobolTreeNode
    {
        protected override int StartIndex
        {
            get { return ParentCobolFile.DivisionsAndSection.WorkingStorage.GetValueOrDefault(-1); }
        }

        protected override int EndIndex
        {
            get { return ParentCobolFile.DivisionsAndSection.Linkage.GetValueOrDefault(-1); }
        }

        public override List<CobolTreeNode> GetNodes()
        {
            return new List<CobolTreeNode>();
        }

        public WorkingStorageSection(CobolFile cobolFile) : base(cobolFile, "Working-Storage Section")
        {
        }
    }
}
