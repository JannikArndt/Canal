namespace Canal.CobolTree.Models
{
    public class IdentificationDivision
    {
        public string OriginalSource { get; set; }

        public IdentificationDivision(string sourceCode)
        {
            this.OriginalSource = sourceCode;
        }
    }
}
