using System.Collections.Generic;

namespace Model
{
    public class WorkingStorageSection : Section
    {
        public override List<CobolTreeNode> GetNodes()
        {
            return new List<CobolTreeNode>();
        }

        public WorkingStorageSection(CobolFile cobolFile) : base(
            cobolFile,
            "Working-Storage Section",
            cobolFile.DivisionsAndSection.WorkingStorage.GetValueOrDefault(-1),
            cobolFile.DivisionsAndSection.Linkage.GetValueOrDefault(-1))
        {
        }
    }
}
