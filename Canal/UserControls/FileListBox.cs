using System.Drawing;
using System.IO;
using System.Windows.Forms;

namespace Canal.UserControls
{
    public partial class FileListBox : ListBox
    {
        public FileListBox()
        {
            InitializeComponent();
        }

        protected override void OnDrawItem(DrawItemEventArgs e)
        {
            base.OnDrawItem(e);
            e.DrawBackground();

            if (e.Index < 0 || Items.Count == 0)
                return;

            var text = Items[e.Index].ToString();

            var bounds = e.Bounds;
            bounds.Y += 3;


            var path = Path.GetDirectoryName(text) + "\\";
            var pathBounds = bounds;
            var w = e.Graphics.MeasureString(path, e.Font).Width;
            pathBounds.Width = (int)w + 1;
            e.Graphics.DrawString(path, e.Font, Brushes.DarkGray, pathBounds, StringFormat.GenericDefault);

            var filename = Path.GetFileNameWithoutExtension(text);
            var filenameBounds = bounds;
            filenameBounds.X += pathBounds.Width - 10;
            filenameBounds.Width = (int)e.Graphics.MeasureString(filename, e.Font).Width + 1;
            e.Graphics.DrawString(filename, e.Font, Brushes.Black, filenameBounds, StringFormat.GenericDefault);

            var extension = Path.GetExtension(text);
            var extensionBounds = bounds;
            extensionBounds.X += pathBounds.Width + filenameBounds.Width - 14;
            extensionBounds.Width = (int)e.Graphics.MeasureString(extension, e.Font).Width + 1;
            e.Graphics.DrawString(extension, e.Font, Brushes.DarkGray, extensionBounds, StringFormat.GenericDefault);

            e.DrawFocusRectangle();
        }

        protected override void OnMeasureItem(MeasureItemEventArgs e)
        {
            base.OnMeasureItem(e);
            e.ItemHeight += 6;
        }
    }
}
