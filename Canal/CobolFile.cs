using Canal.Utils;

namespace Canal
{
    using CobolTree;
    using System.Collections.Generic;

    public class CobolFile
    {
        public string Name;

        public string Text { get; set; }

        public FileReference FileReference { get; set; }

        public Dictionary<string, string> Infos { get; set; }

        public List<Variable> Variables { get { return CobolTree.Variables; } }

        public CobolTree.CobolTree CobolTree { get; set; }

        public CobolFile(string text, string name)
        {
            Name = name;
            Text = text;
            CobolTree = new CobolTree.CobolTree(Text, Name);
            Infos = new Dictionary<string, string>
            {
                {"Name", Name },
                {"Lines of Code", CobolTree.LinesOfCode.ToString() }
            };
        }

        public void RebuildTree()
        {
            CobolTree = new CobolTree.CobolTree(Text, Name);
        }
    }
}