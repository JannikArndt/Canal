using System.Collections.Generic;

namespace Model
{
    public class DataDivision : Division
    {
        public WorkingStorageSection WorkingStorageSection { set; get; }

        public LinkageSection LinkageSection { set; get; }

        protected override int StartIndex
        {
            get { return ParentCobolFile.DivisionsAndSection.Data.GetValueOrDefault(-1); }
        }

        protected override int EndIndex
        {
            get { return ParentCobolFile.DivisionsAndSection.Procedure.GetValueOrDefault(-1); }
        }

        public override List<CobolTreeNode> GetNodes()
        {
            return new List<CobolTreeNode> { WorkingStorageSection, LinkageSection };
        }

        public DataDivision(CobolFile cobolFile) : base(cobolFile, "Data Division")
        {
        }
    }
}
