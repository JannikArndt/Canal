namespace Canal.CobolTree
{
    public class IdentificationDivision : CobolTreeNode
    {
        public string OriginalSource { get; set; }

        public IdentificationDivision(string sourceCode, int indexIdentificationDivision)
            : base("Identification Division", indexIdentificationDivision)
        {
            OriginalSource = sourceCode;

            // AUTHOR, INSTALLATION, DATE-WRITTEN; DATE-COMPILED; SECURITY
        }
    }
}
