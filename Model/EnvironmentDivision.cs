using System.Collections.Generic;

namespace Model
{
    public class EnvironmentDivision : Division
    {
        public EnvironmentDivision(CobolFile cobolFile) : base(cobolFile, "Environment Division")
        {
        }

        protected override int StartIndex
        {
            get { return ParentCobolFile.DivisionsAndSection.Environment.GetValueOrDefault(-1); }
        }

        protected override int EndIndex
        {
            get { return ParentCobolFile.DivisionsAndSection.Data.GetValueOrDefault(-1); }
        }

        public override List<CobolTreeNode> GetNodes()
        {
            return new List<CobolTreeNode>();
        }
    }
}
