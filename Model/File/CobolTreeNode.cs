using System.Collections.Generic;

namespace Model.File
{
    /// <summary>
    /// Represents a part of a COBOL program, i.e. a division, section or procedure
    /// </summary>
    public abstract class CobolTreeNode
    {
        /// <summary>
        /// Reference to the <see cref="CobolFile"/> containing this node
        /// </summary>
        public CobolFile ParentCobolFile { get; private set; }

        /// <summary>
        /// First character of this node
        /// </summary>
        public abstract int StartIndex { get; protected set; }

        /// <summary>
        /// Last character of this node
        /// </summary>
        public abstract int EndIndex { get; protected set; }

        /// <summary>
        /// Name of this part, e.g. "Working-Storage Section" or "SOME-PROCEDURE-NAME"
        /// </summary>
        public string Name { get; private set; }

        /// <summary>
        /// Amount of characters in this node
        /// </summary>
        public int Length { get { return EndIndex - StartIndex; } }

        /// <summary>
        /// Sets the parent reference and name
        /// </summary>
        /// <param name="cobolFile">reference to a CobolFile</param>
        /// <param name="nodeName">Name of the Node</param>
        protected CobolTreeNode(CobolFile cobolFile, string nodeName)
        {
            ParentCobolFile = cobolFile;
            Name = nodeName;
        }

        /// <summary>
        /// Returns the part of the referenced <see cref="CobolFile"/> that this node represents
        /// </summary>
        /// <returns></returns>
        public string GetCode()
        {
            if (StartIndex < 0 || EndIndex < 0)
                return string.Empty;

            return ParentCobolFile.Text.Substring(StartIndex, EndIndex - StartIndex);
        }

        public abstract List<CobolTreeNode> GetNodes();
    }
}
