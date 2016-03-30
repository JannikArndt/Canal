using System.Collections.Generic;

namespace Model
{
    public class LinkageSection : CobolTreeNode
    {
        public List<Variable> Variables { get; set; }

        public string OriginalSource { get; set; }

        public LinkageSection(string sourceCode, int indexInSource)
            : base("Linkage Section", indexInSource)
        {
            OriginalSource = sourceCode;
        }
    }
}
