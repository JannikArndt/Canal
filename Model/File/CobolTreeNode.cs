using System.Collections.Generic;

namespace Model.File
{
    public abstract class CobolTreeNode
    {
        public CobolFile ParentCobolFile { get; private set; }

        public abstract int StartIndex { get; protected set; }
        public abstract int EndIndex { get; protected set; }

        public string Name { get; set; }

        public int Length { get { return EndIndex - StartIndex; } }

        protected CobolTreeNode(CobolFile cobolFile, string nodeText)
        {
            ParentCobolFile = cobolFile;
            Name = nodeText;
        }

        public string GetCode()
        {
            if (StartIndex < 0 || EndIndex < 0)
                return string.Empty;

            return ParentCobolFile.Text.Substring(StartIndex, EndIndex - StartIndex);
        }

        public abstract List<CobolTreeNode> GetNodes();
    }
}
