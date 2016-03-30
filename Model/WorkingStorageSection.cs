using System.Collections.Generic;

namespace Model
{
    public class WorkingStorageSection : CobolTreeNode
    {
        public List<Variable> Variables { get; set; }

        public string OriginalSource { get; set; }

        public WorkingStorageSection(string sourceCode, int indexInSource)
            : base("Working-Storage Section", indexInSource)
        {
            OriginalSource = sourceCode;
        }
    }
}
