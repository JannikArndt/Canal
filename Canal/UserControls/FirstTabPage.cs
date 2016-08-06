using Canal.Properties;
using Logging;
using System;
using System.Drawing;
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

            recentFilesListView.MeasureItem += RecentFilesListViewOnMeasureItem;
            recentFilesListView.DrawItem += RecentFilesListViewOnDrawItem;

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

        private void RecentFilesListViewOnDrawItem(object sender, DrawItemEventArgs drawItemEventArgs)
        {
            if (drawItemEventArgs.Index < 0)
                return;

            var color = (drawItemEventArgs.State & DrawItemState.Selected) == DrawItemState.Selected ? Brushes.Gray : Brushes.Black;

            drawItemEventArgs.Graphics.FillRectangle(new SolidBrush(((ListBox)sender).BackColor), drawItemEventArgs.Bounds);
            drawItemEventArgs.Graphics.DrawString(((ListBox)sender).Items[drawItemEventArgs.Index].ToString(),
                Font, color, drawItemEventArgs.Bounds.X, drawItemEventArgs.Bounds.Y + 3);
            drawItemEventArgs.DrawFocusRectangle();
        }

        private void RecentFilesListViewOnMeasureItem(object sender, MeasureItemEventArgs measureItemEventArgs)
        {
            measureItemEventArgs.ItemHeight += 6;
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            ShowRecentFiles();
            base.OnPaint(e);
        }

        private void ShowRecentFiles()
        {
            var files = MostRecentlyUsed.Instance.GetFiles();

            if (files.None() && Util.Properties.Settings.Default.RecentFiles != null &&
                Util.Properties.Settings.Default.RecentFiles.Count > 0)
            {
                MostRecentlyUsed.Instance.Import(Util.Properties.Settings.Default.RecentFiles);
                files = MostRecentlyUsed.Instance.GetFiles();
            }

            recentFilesListView.Items.Clear();

            foreach (var file in files)
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
