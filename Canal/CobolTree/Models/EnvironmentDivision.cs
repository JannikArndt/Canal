namespace Canal.CobolTree.Models
{
    public class EnvironmentDivision
    {
        public string OriginalSource { get; set; }

        public EnvironmentDivision(string sourceCode)
        {
            this.OriginalSource = sourceCode;
        }
    }
}
