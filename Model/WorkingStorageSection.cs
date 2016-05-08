using Model.References;
using System.Collections.Generic;

namespace Model
{
    public class WorkingStorageSection : CobolTreeNode
    {
        public List<Variable> Variables { get; set; }

        public void SetCopyReferences(IEnumerable<FileReference> value)
        {
            foreach (var fileReference in value)
            {
                Nodes.Add("COPY " + fileReference.ProgramName);
            }
        }

        public string OriginalSource { get; set; }

        public WorkingStorageSection(string sourceCode, int indexInSource)
            : base("Working-Storage Section", indexInSource)
        {
            OriginalSource = sourceCode;
        }
    }
}
