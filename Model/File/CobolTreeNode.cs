using System.Collections.Generic;

namespace Model.File
{
    public abstract class CobolTreeNode
    {
        public CobolFile ParentCobolFile { get; }

        public abstract int StartIndex { get; }
        public abstract int EndIndex { get; }

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
