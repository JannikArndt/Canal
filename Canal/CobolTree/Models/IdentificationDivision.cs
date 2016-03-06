using System.Windows.Forms;

namespace Canal.CobolTree.Models
{
    public class IdentificationDivision : TreeNode
    {
        public string OriginalSource { get; set; }

        public IdentificationDivision(string sourceCode) : base("Identification Division")
        {
            this.OriginalSource = sourceCode;
        }
    }
}
