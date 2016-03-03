namespace Canal.CobolTree.Models
{
    using System.Collections.Generic;

    public class Section
    {
        public Section(string name, string text)
        {
            this.Name = name;
            this.OriginalSource = text;
        }

        public string Name { get; set; }

        public string OriginalSource { get; set; }

        public List<Section> PerformReferences { get; set; }

        public List<FileReference> CallReferences { get; set; }

        public List<FileReference> CopyReferences { get; set; }
    }
}
