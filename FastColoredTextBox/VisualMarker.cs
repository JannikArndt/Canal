using System.Drawing;
using System.Windows.Forms;

namespace FastColoredTextBoxNS
{
    public class VisualMarker
    {
        public readonly Rectangle rectangle;

        public VisualMarker(Rectangle rectangle)
        {
            this.rectangle = rectangle;
        }

        public virtual void Draw(Graphics gr, Pen pen)
        {
        }

        public virtual Cursor Cursor
        {
            get { return Cursors.Hand; }
        }
    }
}
