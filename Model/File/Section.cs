using System.Collections.Generic;

namespace Model.File
{
    /// <summary>
    /// Represents a SECTION
    /// </summary>
    public class Section : Procedure
    {
        /// <summary>
        /// List of all PROCEDUREs in this SECTION
        /// </summary>
        public List<Procedure> Procedures { get; set; }

        public Section(CobolFile cobolFile, string name, int startIndex, int endIndex)
            : base(cobolFile, name, startIndex, endIndex)
        {
            Procedures = new List<Procedure>();
        }

        public override List<CobolTreeNode> GetNodes()
        {
            return new List<CobolTreeNode>(Procedures);
        }
    }
}
