using System.Windows.Forms;

namespace Canal
{
    public partial class Log : Form
    {
        public Log(string text)
        {
            InitializeComponent();

            LogTextBox.Text = text;
            LogTextBox.Select(text.Length - 1, 1);
            LogTextBox.ScrollToCaret();
        }
    }
}
