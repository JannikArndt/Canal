using Canal.Properties;
using Logging;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;
using Util;

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
                ShowRecentFiles();

                changeLogTextBox.Text = Resources.ChangeLog;
            }
            catch (Exception exception)
            {
                Logger.Error("Error on startup (FirstTabPage) {0}: {1}", exception.GetType(), exception.Message);
            }
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            ShowRecentFiles();
            base.OnPaint(e);
        }

        private void ShowRecentFiles()
        {
            recentFilesListView.Items.Clear();
            var hashSet = new HashSet<string>();
            if (Util.Properties.Settings.Default.RecentFiles != null)
                foreach (var file in Util.Properties.Settings.Default.RecentFiles)
                    hashSet.Add(file);

            foreach (var file in hashSet.OrderBy(item => item))
                recentFilesListView.Items.Add(file);
        }

        private void recentFilesListView_DoubleClick(object sender, EventArgs e)
        {
            _parent.OpenFile(recentFilesListView.SelectedItems[0].ToString());

        }


    }
}
