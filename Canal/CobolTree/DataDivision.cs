namespace Canal.CobolTree
{
    using System;
    using System.Collections.Generic;
    using System.Linq;

    public class DataDivision : Division
    {
        public List<Variable> Variables
        {
            get
            {
                return WorkingStorageSection.Variables.Union(LinkageSection.Variables).ToList();
            }
        }

        public DataDivision(string sourceCode, int indexDataDivision)
            : base(sourceCode, "Data Division", indexDataDivision)
        {
            int indexWorkingStorageSection = Math.Max(0, sourceCode.IndexOf("WORKING-STORAGE SECTION", StringComparison.Ordinal));
            int indexLinkageSection = Math.Max(0, sourceCode.IndexOf("LINKAGE SECTION", StringComparison.Ordinal));

            WorkingStorageSection = new WorkingStorageSection(
                sourceCode.Substring(indexWorkingStorageSection, Math.Max(0, indexLinkageSection - indexWorkingStorageSection)),
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
