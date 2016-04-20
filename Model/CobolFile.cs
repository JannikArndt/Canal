using System.Collections.Generic;
using Model.References;

namespace Model
{
    public class CobolFile
    {
        public string Name { get; set; }

        public string Text { get; set; }

        public FileReference FileReference { get; set; }

        public Dictionary<string, string> Infos { get; set; }

        public List<FileReference> CallReferences { get { return CobolTree == null ? new List<FileReference>() : CobolTree.CallReferences; } }

        public CobolTree CobolTree { get; set; }

        public CobolFile(string text, string name)
        {
            Name = name;
            Text = text;
        }
    }
}