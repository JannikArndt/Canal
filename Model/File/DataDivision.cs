using System.Collections.Generic;

namespace Model.File
{
    /// <summary>
    /// Represents the DATA DIVISION
    /// </summary>
    public class DataDivision : Division
    {
        public WorkingStorageSection WorkingStorageSection { set; get; }

        public LinkageSection LinkageSection { set; get; }

        public override List<CobolTreeNode> GetNodes()
        {
            return new List<CobolTreeNode> { WorkingStorageSection, LinkageSection };
        }

        public DataDivision(CobolFile cobolFile) : base(cobolFile, "Data Division",
            cobolFile.DivisionsAndSection.Data.GetValueOrDefault(-1),
            cobolFile.DivisionsAndSection.Procedure.GetValueOrDefault(-1))
        {
        }
    }
}
