namespace Canal
{
    using CobolTree;
    using System.Collections.Generic;

    public class CobolFile
    {
        internal string Name;

        private CobolTree.CobolTree _cobolTree;

        public string Text { get; set; }

        public List<Variable> Variables
        {
            get
            {
                return CobolTree.Variables;
            }
        }

        public CobolTree.CobolTree CobolTree
        {
            get { return _cobolTree ?? (_cobolTree = new CobolTree.CobolTree(Text, Name)); }
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
            _cobolTree = new CobolTree.CobolTree(Text, Name);
        }
    }
}