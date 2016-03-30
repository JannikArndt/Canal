using Canal.Properties;
using Canal.UserControls;
using Model;
using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.Linq;
using System.Windows.Forms;

namespace Canal
{
    using Level88ToEnum;
    using Utils;

    public partial class MainWindow : Form
    {
        private readonly TabUtil _tabUtil;

        public MainWindow(string[] files = null)
        {
            InitializeComponent();

            ErrorHandling.Start();

            _tabUtil = new TabUtil(FileTabs, this);
            try
            {
                var toOpen = new List<string>();
                if (files != null) toOpen.AddRange(files);
                if (Settings.Default.LastOpened != null) toOpen.AddRange(Settings.Default.LastOpened.Cast<string>());

                foreach (string filepath in new HashSet<string>(toOpen))
                    OpenFile(filepath);
            }
            catch (Exception exception)
            {
                ErrorHandling.Exception(exception);
                MessageBox.Show(Resources.ErrorMessage_MainWindow_OpenPrevious + exception.Message, Resources.Error, MessageBoxButtons.OK);
            }
        }

        protected override void OnClosing(CancelEventArgs e)
        {
            Settings.Default.LastOpened = new StringCollection();
            Settings.Default.LastOpened.AddRange(_tabUtil.GetOpenFiles().Select(file => file.FileReference.FullPath).ToArray());
            Settings.Default.Save();
            ErrorHandling.End();
            base.OnClosing(e);
        }

        public void OpenFile(string filename)
        {
            if (_tabUtil.TryShowTab(filename))
                return;

            Cursor = Cursors.WaitCursor;

            try
            {
                var file = FileUtil.Get(filename);
                _tabUtil.AddTab(file);
            }
            catch (Exception exception)
            {
                ErrorHandling.Exception(exception);
                MessageBox.Show(Resources.ErrorMessage_MainWindow_ErrorLoadingFile + exception.Message, Resources.Error, MessageBoxButtons.OK);
            }
            finally
            {
                Cursor = Cursors.Default;
            }
        }

        private void openToolStripMenuItem_Click(object sender, EventArgs e)
        {
            openFileDialog.Filter = @"COBOL Files|*.cob;*.cbl;*.txt;.src";
            openFileDialog.FileName = "";

            var dialogResult = openFileDialog.ShowDialog();

            if (dialogResult == DialogResult.OK)
            {
                OpenFile(openFileDialog.FileName);
            }
        }

        private void closeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            _tabUtil.CloseTab();
        }

        private void level88ToEnumConverterToolStripMenuItem_Click(object sender, EventArgs e)
        {
            var converterWindow = new Level88ToEnum();
            converterWindow.Show();
        }

        private void exitToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_tabUtil.CloseAllTabs())
                Close();
        }

        private void newToolStripMenuItem_Click(object sender, EventArgs e)
        {
            _tabUtil.AddTab(new CobolFile("", "New File"));
        }

        private void reportIssueToolStripMenuItem_Click(object sender, EventArgs e)
        {
            Process.Start("https://github.com/JannikArndt/Canal/issues/new");
        }

        private void aboutToolStripMenuItem_Click(object sender, EventArgs e)
        {
            var about = new About();
            about.Show();
        }
    }
}
