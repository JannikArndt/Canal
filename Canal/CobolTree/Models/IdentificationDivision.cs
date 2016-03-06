namespace Canal.CobolTree.Models
{
    public class IdentificationDivision : CobolTreeNode
    {
        public string OriginalSource { get; set; }

        public IdentificationDivision(string sourceCode, int indexIdentificationDivision) : base("Identification Division", indexIdentificationDivision)
        {
            OriginalSource = sourceCode;
        }
    }
}
