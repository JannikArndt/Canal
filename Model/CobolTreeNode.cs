using System.Collections.Generic;

namespace Model
{
    public abstract class CobolTreeNode
    {
        protected CobolFile ParentCobolFile { get; }

        protected abstract int StartIndex { get; }
        protected abstract int EndIndex { get; }

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
