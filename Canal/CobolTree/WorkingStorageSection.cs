namespace Canal.CobolTree
{
    using System.Collections.Generic;

    using Canal.Utils;

    public class WorkingStorageSection : CobolTreeNode
    {
        public List<Variable> Variables { get; set; }

        public string OriginalSource { get; set; }

        public WorkingStorageSection(string sourceCode, int indexInSource)
            : base("Working-Storage Section", indexInSource)
        {
            this.OriginalSource = sourceCode;
            this.Variables = VariablesUtil.AnalyzeVariables(sourceCode);
        }
    }
}
