using Canal.Properties;
using Logging;
using System;
using System.Collections.Generic;
using System.Windows.Forms;

namespace Canal.UserControls
{
    public partial class FirstTabPage : UserControl
    {
        private readonly MainWindow _parent;

        public FirstTabPage(MainWindow parent)
        {
            InitializeComponent();

            _parent = parent;

            try
            {
                var hashSet = new HashSet<string>();
                foreach (var file in Settings.Default.LastOpened)
                {
                    hashSet.Add(file);
                }

                foreach (var file in hashSet)
                    recentFilesListView.Items.Add(file);

                changeLogTextBox.Text = Resources.ChangeLog;
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
            foreach (var file in Settings.Default.LastOpened)
                _parent.OpenFile(file);
        }
    }
}
