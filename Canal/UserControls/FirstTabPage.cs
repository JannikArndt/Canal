using Canal.Properties;
using Logging;
using System;
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
                Logger.Error(exception, "Error on startup (FirstTabPage) {0}: {1}", exception.GetType(), exception.Message);
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

            foreach (var file in MostRecentlyUsed.Instance.GetFiles())
            {
                recentFilesListView.Items.Add(file);
            }
        }

        private void recentFilesListView_DoubleClick(object sender, EventArgs e)
        {
            _parent.OpenFile(recentFilesListView.SelectedItems[0].ToString());

        }


    }
}
