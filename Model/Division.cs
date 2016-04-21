namespace Model
{
    /// <summary>
    /// Abstract class for Identification/Environment/Data/Procedure Division
    /// </summary>
    public abstract class Division : CobolTreeNode
    {
        protected Division(string sourceCode, string nodeName, int indexDataDivision)
            : base(nodeName, indexDataDivision)
        {
            OriginalSource = sourceCode;
        }

        /// <summary>
        /// Original source code of this division
        /// </summary>
        public string OriginalSource { get; private set; }
    }
}