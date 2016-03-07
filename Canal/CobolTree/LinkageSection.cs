namespace Canal.CobolTree
{
    using System.Collections.Generic;

    using Canal.Utils;

    public class LinkageSection : CobolTreeNode
    {
        public List<Variable> Variables { get; set; }

        public string OriginalSource { get; set; }

        public LinkageSection(string sourceCode, int indexInSource)
            : base("Linkage Section", indexInSource)
        {
            this.OriginalSource = sourceCode;
            this.Variables = VariablesUtil.AnalyzeVariables(sourceCode);
        }
    }
}
