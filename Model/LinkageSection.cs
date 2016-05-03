using Model.References;
using System.Collections.Generic;

namespace Model
{
    public class LinkageSection : CobolTreeNode
    {
        private List<FileReference> copyReferences = new List<FileReference>();

        public List<Variable> Variables { get; set; }

        public List<FileReference> CopyReferences
        {
            get
            {
                return copyReferences;
            }
            set
            {
                foreach (var fileReference in value)
                {
                    Nodes.Add("COPY " + fileReference.ProgramName);
                }

                copyReferences = value;
            }
        }

        public string OriginalSource { get; set; }

        public LinkageSection(string sourceCode, int indexInSource)
            : base("Linkage Section", indexInSource)
        {
            OriginalSource = sourceCode;
        }
    }
}
