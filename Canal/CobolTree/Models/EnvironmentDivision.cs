namespace Canal.CobolTree.Models
{
    public class EnvironmentDivision : CobolTreeNode
    {
        public string OriginalSource { get; set; }

        public EnvironmentDivision(string sourceCode, int indexEnvironmentDivision) : base("Environment Division", indexEnvironmentDivision)
        {
            OriginalSource = sourceCode;
        }
    }
}
