using System.Windows.Forms;

namespace Canal.CobolTree.Models
{
    public class EnvironmentDivision : TreeNode
    {
        public string OriginalSource { get; set; }

        public EnvironmentDivision(string sourceCode) : base("Environment Division")
        {
            this.OriginalSource = sourceCode;
        }
    }
}
