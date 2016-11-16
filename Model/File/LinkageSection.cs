using System.Collections.Generic;

namespace Model.File
{
    /// <summary>
    /// Represents the LINKAGE SECTION
    /// </summary>
    public class LinkageSection : Section
    {
        public override List<CobolTreeNode> GetNodes()
        {
            return new List<CobolTreeNode>();
        }

        public LinkageSection(CobolFile cobolFile) : base(cobolFile, "Linkage Section",
            cobolFile.DivisionsAndSection.Linkage.GetValueOrDefault(-1),
            cobolFile.DivisionsAndSection.Procedure.GetValueOrDefault(-1))
        {
        }
    }
}
