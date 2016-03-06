using System.Windows.Forms;

namespace Canal.CobolTree.Models
{
    public abstract class CobolTreeNode : TreeNode
    {
        public CobolTreeNode(string nodeText, int indexDataDivision) : base(nodeText)
        {
            IndexInSource = indexDataDivision;
        }

        public int IndexInSource { get; set; }
    }
}
