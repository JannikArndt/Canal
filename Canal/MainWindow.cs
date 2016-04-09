using Canal.Properties;
using Canal.UserControls;
using Logging;
using Model;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Windows.Forms;

namespace Canal
{
    using Level88ToEnum;
    using Utils;

    public partial class MainWindow : Form
    {
        private readonly TabUtil _tabUtil;

        private readonly string[] _openFilesOnStartup;

        private ConsoleLogger _log = new ConsoleLogger();

        public MainWindow(string[] files = null)
        {
            InitializeComponent();

            Logger.Singleton.AddMsg(2, "Starting program");

            _openFilesOnStartup = files;

            _tabUtil = new TabUtil(FileTabs, this);
            _tabUtil.ShowStartTab();
        }

        protected override void OnShown(EventArgs e)
        {
            try
            {
                var toOpen = new List<string>();
                if (_openFilesOnStartup != null) toOpen.AddRange(_openFilesOnStartup);

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
            Logger.Singleton.AddMsg(2, "Closing program");
            Settings.Default.Save();
            base.OnClosing(e);
        }

        public void OpenFile(string filename)
        {
            Logger.Singleton.AddMsg(2, "Opening file {0}", filename);

            if (_tabUtil.TryShowTab(filename))
                return;

            if (!File.Exists(filename))
            {
                Logger.Singleton.AddMsg(1, "File could not be found: {0}, showing error message", filename);
                MessageBox.Show(string.Format(Resources.File_Could_Not_Be_Found, filename), Resources.Error, MessageBoxButtons.OK);
                return;
            }

            Cursor = Cursors.WaitCursor;

            try
            {
                var file = FileUtil.Get(filename);
                _tabUtil.AddTab(file);
                Settings.Default.LastOpened.Add(filename);
            }
            catch (Exception exception)
            {
                Logger.Singleton.AddMsg(1, "Error {0}: {1}", exception.GetType(), exception.Message);
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

        private void showLogToolStripMenuItem_Click(object sender, EventArgs e)
        {
            var logWindow = new Log(ConsoleLogger.GetText());
            logWindow.Show();
        }
    }
}
