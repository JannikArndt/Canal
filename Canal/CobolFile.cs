namespace Canal
{
    public class CobolFile
    {
        internal string Name;

        private CobolTree.Models.CobolTree _cobolTree;

        public string Text { get; set; }

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
    }
}