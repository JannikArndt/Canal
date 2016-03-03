
namespace Canal.CobolTree.Models
{
    public class DataDivision
    {
        public string OriginalSource { get; set; }

        public DataDivision(string sourceCode)
        {
            this.OriginalSource = sourceCode;
        }

        public WorkingStorageSection WorkingStorageSection { get; set; }

        public LinkageSection LinkageSection { get; set; }
    }
}
