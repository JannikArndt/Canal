
using System.Windows.Forms;

namespace Canal.CobolTree.Models
{
    public class DataDivision : TreeNode
    {
        public string OriginalSource { get; set; }

        public DataDivision(string sourceCode) : base("Data Division")
        {
            OriginalSource = sourceCode;
            WorkingStorageSection = new WorkingStorageSection();
            LinkageSection = new LinkageSection();
            Nodes.Add(WorkingStorageSection);
            Nodes.Add(LinkageSection);
        }

        public WorkingStorageSection WorkingStorageSection { get; set; }

        public LinkageSection LinkageSection { get; set; }
    }
}
