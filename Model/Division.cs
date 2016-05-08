namespace Model
{
    /// <summary>
    /// Abstract class for Identification/Environment/Data/Procedure Division
    /// </summary>
    public abstract class Division : CobolTreeNode
    {
        protected Division(CobolFile cobolFile, string nodeName)
            : base(cobolFile, nodeName)
        {

        }
    }
}