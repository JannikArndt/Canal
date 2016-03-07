namespace Canal.CobolTree
{
    using System.Windows.Forms;

    public abstract class CobolTreeNode : TreeNode
    {
        public CobolTreeNode(string nodeText, int indexDataDivision)
            : base(nodeText)
        {
            this.IndexInSource = indexDataDivision;
        }

        public int IndexInSource { get; set; }
    }
}
