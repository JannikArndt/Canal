namespace Canal.CobolTree
{
    using System.Windows.Forms;

    public abstract class CobolTreeNode : TreeNode
    {
        public CobolTreeNode()
        {
        }

        public CobolTreeNode(string nodeText, int indexDataDivision)
            : base(nodeText)
        {
            IndexInSource = indexDataDivision;
        }

        public int IndexInSource { get; set; }
    }
}
