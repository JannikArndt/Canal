using System;
using System.Deployment.Application;
using System.Diagnostics;
using System.Windows.Forms;

namespace Canal.UserControls
{
    public partial class About : Form
    {
        public About()
        {
            InitializeComponent();

            var version = ApplicationDeployment.IsNetworkDeployed
                ? ApplicationDeployment.CurrentDeployment.CurrentVersion
                : new Version(2, 1, 0, 0);

            versiontextBox.Text = versiontextBox.Text.Replace("#version#", version.ToString());
        }

        private void GoToGitHub(object sender, EventArgs e)
        {
            Process.Start("https://github.com/JannikArndt/Canal");
        }

        private void GoToFCTB(object sender, EventArgs e)
        {
            Process.Start("https://github.com/PavelTorgashov/FastColoredTextBox");

        }

        private void GoToSourceCodePro(object sender, EventArgs e)
        {
            Process.Start("https://github.com/adobe-fonts/source-code-pro");
        }
    }
}
