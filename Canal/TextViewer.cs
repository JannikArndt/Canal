using System.Windows.Forms;

namespace Canal
{
    public partial class TextViewer : Form
    {
        public TextViewer(string text)
        {
            InitializeComponent();

            fastColoredTextBox1.Text = text;
        }
    }
}
