using Model.References;
using System.Collections.Generic;

namespace Model
{
    public class WorkingStorageSection : CobolTreeNode
    {
        private List<FileReference> _copyReferences = new List<FileReference>();

        public List<Variable> Variables { get; set; }

        public List<FileReference> CopyReferences
        {
            get
            {
                return _copyReferences;
            }
            set
            {
                foreach (var fileReference in value)
                {
                    Nodes.Add("COPY " + fileReference.ProgramName);
                }

                _copyReferences = value;
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
