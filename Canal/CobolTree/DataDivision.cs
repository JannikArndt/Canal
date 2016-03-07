namespace Canal.CobolTree
{
    using System;
    using System.Collections.Generic;
    using System.Linq;

    public class DataDivision : CobolTreeNode
    {
        public string OriginalSource { get; set; }

        public List<Variable> Variables
        {
            get
            {
                return WorkingStorageSection.Variables.Union(LinkageSection.Variables).ToList();
            }
        }

        public DataDivision(string sourceCode, int indexDataDivision)
            : base("Data Division", indexDataDivision)
        {
            OriginalSource = sourceCode;

            int indexWorkingStorageSection = Math.Max(0, sourceCode.IndexOf("WORKING-STORAGE SECTION", StringComparison.Ordinal));
            int indexLinkageSection = Math.Max(0, sourceCode.IndexOf("LINKAGE SECTION", StringComparison.Ordinal));

            WorkingStorageSection = new WorkingStorageSection(
                sourceCode.Substring(indexWorkingStorageSection, indexLinkageSection - indexWorkingStorageSection),
                indexDataDivision + indexWorkingStorageSection);

            LinkageSection = new LinkageSection(
                sourceCode.Substring(indexLinkageSection, sourceCode.Length - indexLinkageSection),
                indexDataDivision + indexLinkageSection);

            Nodes.Add(WorkingStorageSection);
            Nodes.Add(LinkageSection);
        }

        public WorkingStorageSection WorkingStorageSection { get; set; }

        public LinkageSection LinkageSection { get; set; }
    }
}
