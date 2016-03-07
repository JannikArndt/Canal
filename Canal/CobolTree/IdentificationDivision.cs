namespace Canal.CobolTree
{
    public class IdentificationDivision : CobolTreeNode
    {
        public string OriginalSource { get; set; }

        public IdentificationDivision(string sourceCode, int indexIdentificationDivision)
            : base("Identification Division", indexIdentificationDivision)
        {
            this.OriginalSource = sourceCode;
        }
    }
}
