using Logging;
using System;
using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;

namespace Canal.UserControls
{
    public partial class FirstTabPage : UserControl
    {
        private readonly MainWindow _parent;

        private readonly List<string> _recentFilesList;

        public FirstTabPage(List<string> recentFilesList, MainWindow parent)
        {
            InitializeComponent();

            _parent = parent;

            _recentFilesList = recentFilesList;

            try
            {
                foreach (var file in recentFilesList)
                    recentFilesListView.Items.Add(file);

                changeLogTextBox.Text = File.ReadAllText("Resources/ChangeLog.txt");
            }
            catch (Exception exception)
            {
                Logger.Singleton.AddMsg(1, "Error on startup (FirstTabPage) {0}: {1}", exception.GetType(), exception.Message);
                MessageBox.Show("Error: " + exception.Message, "Error", MessageBoxButtons.OK);
            }
        }

        private void recentFilesListView_DoubleClick(object sender, EventArgs e)
        {
            _parent.OpenFile(recentFilesListView.SelectedItems[0].Text);

        }

        private void OpenAllRecentFiles(object sender, EventArgs e)
        {
            foreach (var file in _recentFilesList)
                _parent.OpenFile(file);
        }
    }
}
