namespace Canal.CobolTree
{
    using System.Collections.Generic;

    using Utils;

    public class LinkageSection : CobolTreeNode
    {
        public List<Variable> Variables { get; set; }

        public string OriginalSource { get; set; }

        public LinkageSection(string sourceCode, int indexInSource)
            : base("Linkage Section", indexInSource)
        {
            OriginalSource = sourceCode;
            Variables = VariablesUtil.AnalyzeVariables(sourceCode);
        }
    }
}
