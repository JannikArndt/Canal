using System.Diagnostics;
using System.IO;
using System.Windows.Forms;

namespace Canal.UserControls
{
    public partial class About : Form
    {
        public About()
        {
            InitializeComponent();

            var rtf = File.ReadAllText("UserControls/AboutText.rtf");
            richTextBox1.Rtf = rtf;
            richTextBox1.LinkClicked += Link;
        }

        private void Link(object sender, LinkClickedEventArgs linkClickedEventArgs)
        {
            Process.Start(linkClickedEventArgs.LinkText);
        }
    }
}
