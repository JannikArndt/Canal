using System.Collections.Generic;

namespace Model.File
{
    /// <summary>
    /// Represents the ENVIRONMENT DIVISION
    /// </summary>
    public class EnvironmentDivision : Division
    {
        public EnvironmentDivision(CobolFile cobolFile) : base(cobolFile, "Environment Division",
            cobolFile.DivisionsAndSection.Environment.GetValueOrDefault(-1),
            cobolFile.DivisionsAndSection.Data.GetValueOrDefault(-1))
        {
        }

        public override List<CobolTreeNode> GetNodes()
        {
            return new List<CobolTreeNode>();
        }
    }
}
