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
                return this.WorkingStorageSection.Variables.Union(this.LinkageSection.Variables).ToList();
            }
        }

        public DataDivision(string sourceCode, int indexDataDivision)
            : base("Data Division", indexDataDivision)
        {
            this.OriginalSource = sourceCode;

            int indexWorkingStorageSection = sourceCode.IndexOf("WORKING-STORAGE SECTION", StringComparison.Ordinal);
            int indexLinkageSection = sourceCode.IndexOf("LINKAGE SECTION", StringComparison.Ordinal);

            this.WorkingStorageSection = new WorkingStorageSection(
                sourceCode.Substring(indexWorkingStorageSection, indexLinkageSection - indexWorkingStorageSection),
                indexDataDivision + indexWorkingStorageSection);

            this.LinkageSection = new LinkageSection(
                sourceCode.Substring(indexLinkageSection, sourceCode.Length - indexLinkageSection),
                indexDataDivision + indexLinkageSection);

            this.Nodes.Add(this.WorkingStorageSection);
            this.Nodes.Add(this.LinkageSection);
        }

        public WorkingStorageSection WorkingStorageSection { get; set; }

        public LinkageSection LinkageSection { get; set; }
    }
}
