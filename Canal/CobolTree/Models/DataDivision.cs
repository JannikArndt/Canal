using System;

namespace Canal.CobolTree.Models
{
    public class DataDivision : CobolTreeNode
    {
        public string OriginalSource { get; set; }

        public DataDivision(string sourceCode, int indexDataDivision) : base("Data Division", indexDataDivision)
        {
            OriginalSource = sourceCode;

            int indexWorkingStorageSection = sourceCode.IndexOf("WORKING STORANGE SECTION", StringComparison.Ordinal);
            WorkingStorageSection = new WorkingStorageSection(indexDataDivision + indexWorkingStorageSection);

            int indexLinkageSection = sourceCode.IndexOf("LINKAGE SECTION", StringComparison.Ordinal);
            LinkageSection = new LinkageSection(indexLinkageSection);

            Nodes.Add(WorkingStorageSection);
            Nodes.Add(LinkageSection);
        }

        public WorkingStorageSection WorkingStorageSection { get; set; }

        public LinkageSection LinkageSection { get; set; }
    }
}
