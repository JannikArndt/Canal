namespace Model
{
    using System.Collections.Generic;

    using Model.References;

    public class LinkageSection : CobolTreeNode
    {
        private List<FileReference> copyReferences = new List<FileReference>();

        public List<Variable> Variables { get; set; }

        public List<FileReference> CopyReferences
        {
            get
            {
                return this.copyReferences;
            }
            set
            {
                foreach (var fileReference in value)
                {
                    Nodes.Add("COPY " + fileReference.ProgramName);
                }

                this.copyReferences = value;
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
