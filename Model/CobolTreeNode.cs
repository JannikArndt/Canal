using System.Windows.Forms;

namespace Model
{
    public abstract class CobolTreeNode : TreeNode
    {
        protected CobolFile ParentCobolFile { get; }

        protected abstract int StartIndex { get; }
        protected abstract int EndIndex { get; }

        public int Length { get { return EndIndex - StartIndex; } }

        protected CobolTreeNode(CobolFile cobolFile, string nodeText)
            : base(nodeText)
        {
            ParentCobolFile = cobolFile;
        }

        public string GetCode()
        {
            if (StartIndex < 0 || EndIndex < 0)
                return string.Empty;

            return ParentCobolFile.Text.Substring(StartIndex, EndIndex - StartIndex);
        }
    }
}
