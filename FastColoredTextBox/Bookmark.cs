using System.Drawing;
using System.Drawing.Drawing2D;

namespace FastColoredTextBoxNS
{
    /// <summary>
    /// Bookmark of FastColoredTextbox
    /// </summary>
    public class Bookmark
    {
        public FastColoredTextBox TB { get; private set; }
        /// <summary>
        /// Name of bookmark
        /// </summary>
        public string Name { get; set; }
        /// <summary>
        /// Line index
        /// </summary>
        public int LineIndex {get; set; }
        /// <summary>
        /// Color of bookmark sign
        /// </summary>
        public Color Color { get; set; }

        /// <summary>
        /// Scroll textbox to the bookmark
        /// </summary>
        public virtual void DoVisible()
        {
            TB.Selection.Start = new Place(0, LineIndex);
            TB.DoRangeVisible(TB.Selection, true);
            TB.Invalidate();
        }

        public Bookmark(FastColoredTextBox tb, string name, int lineIndex)
        {
            this.TB = tb;
            this.Name = name;
            this.LineIndex = lineIndex;
            Color = tb.BookmarkColor;
        }

        public virtual void Paint(Graphics gr, Rectangle lineRect)
        {
            var size = TB.CharHeight - 1;
            using (var brush = new LinearGradientBrush(new Rectangle(0, lineRect.Top, size, size), Color.White, Color, 45))
                gr.FillEllipse(brush, 0, lineRect.Top, size, size);
            using (var pen = new Pen(Color))
                gr.DrawEllipse(pen, 0, lineRect.Top, size, size);
        }
    }
}