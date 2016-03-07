namespace Canal
{
    using System.Collections.Generic;

    using Canal.CobolTree.Models;

    public class CobolFile
    {
        internal string Name;

        private CobolTree.Models.CobolTree _cobolTree;

        public string Text { get; set; }

        public List<Variable> Variables
        {
            get
            {
                return CobolTree.Variables;
            }
        }

        public CobolTree.Models.CobolTree CobolTree
        {
            get { return _cobolTree ?? (_cobolTree = new CobolTree.Models.CobolTree(Text, Name)); }
            set
            {
                _cobolTree = value;
            }
        }

        public CobolFile(string text, string name)
        {
            Name = name;
            Text = text;
        }

        public void RebuildTree()
        {
            _cobolTree = new CobolTree.Models.CobolTree(Text, Name);
        }
    }
}