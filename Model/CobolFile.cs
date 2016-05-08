using Model.References;
using System.Collections.Generic;

namespace Model
{
    public class CobolFile
    {
        public string Name { get; set; }

        public string Text { get; set; }

        public FileReference FileReference { get; set; }

        public CobolTree CobolTree { get; set; }

        public List<FileReference> CopyReferences { get; set; }

        public CobolFile(string text, string name)
        {
            Name = name;
            Text = text;
        }
    }
}