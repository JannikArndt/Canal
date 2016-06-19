using Canal.UserControls;
using System.Collections.Specialized;
using System.Diagnostics;
using System.Net;

namespace Canal
{
    using Logging;
    using Model;
    using Properties;
    using System;
    using System.Collections.Generic;
    using System.ComponentModel;
    using System.IO;
    using System.Windows.Forms;

    public partial class MainWindow : Form
    {
        private readonly TabUtil _tabUtil;

        private readonly string[] _openFilesOnStartup;

        public MainWindow(string[] files = null)
        {
            InitializeComponent();

            Logger.Info("Starting program");

            _openFilesOnStartup = files;

            if (Util.Properties.Settings.Default.RecentFiles == null)
                Util.Properties.Settings.Default.RecentFiles = new StringCollection();

            _tabUtil = new TabUtil(FileTabs, this);
            _tabUtil.ShowStartTab();
        }

        #region Overrides

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
                Logger.Error("Error trying to open files on startup {0}: {1}.",
                    _openFilesOnStartup == null ? "_openFilesOnStartup is null" : string.Join(", ", _openFilesOnStartup),
                    exception.Message);
                MessageBox.Show(Resources.ErrorMessage_MainWindow_OpenPrevious + exception.Message, Resources.Error, MessageBoxButtons.OK);
            }
        }

        protected override void OnClosing(CancelEventArgs e)
        {
            if (!_tabUtil.CloseAllTabs())
            {
                e.Cancel = true;
                return;
            }

            Logger.Info("Closing program");
            Settings.Default.Save();
            Util.Properties.Settings.Default.Save();
            base.OnClosing(e);
        }

        protected override bool ProcessCmdKey(ref Message msg, Keys keyData)
        {
            if (keyData == (Keys.Control | Keys.Tab))
            {
                _tabUtil.ShowNextTab();
                return true;
            }
            if (keyData == (Keys.Control | Keys.Shift | Keys.Tab))
            {
                _tabUtil.ShowPreviousTab();
                return true;
            }
            if (keyData == (Keys.Alt | Keys.D1))
            {
                _tabUtil.TryShowTab(1);
                return true;
            }
            if (keyData == (Keys.Alt | Keys.D2))
            {
                _tabUtil.TryShowTab(2);
                return true;
            }
            if (keyData == (Keys.Alt | Keys.D3))
            {
                _tabUtil.TryShowTab(3);
                return true;
            }
            if (keyData == (Keys.Alt | Keys.D4))
            {
                _tabUtil.TryShowTab(4);
                return true;
            }
            if (keyData == (Keys.Alt | Keys.D5))
            {
                _tabUtil.TryShowTab(5);
                return true;
            }
            if (keyData == (Keys.Alt | Keys.D6))
            {
                _tabUtil.TryShowTab(6);
                return true;
            }
            if (keyData == (Keys.Alt | Keys.D7))
            {
                _tabUtil.TryShowTab(7);
                return true;
            }
            if (keyData == (Keys.Alt | Keys.D8))
            {
                _tabUtil.TryShowTab(8);
                return true;
            }
            if (keyData == (Keys.Alt | Keys.D9))
            {
                _tabUtil.TryShowTab(9);
                return true;
            }
            if (keyData == (Keys.Alt | Keys.D0))
            {
                _tabUtil.TryShowTab(0);
                return true;
            }
            if (keyData == Keys.F1)
            {
                var logWindow = new Log();
                logWindow.Show();
                return true;
            }
            if (keyData == Keys.F4)
            {
                ShowCodeGenerator();
                return true;
            }
            return base.ProcessCmdKey(ref msg, keyData);
        }

        #endregion

        #region File Actions

        public void New()
        {
            _tabUtil.AddTab();
        }

        public void OpenFile(string filename, Variable currentVar = null)
        {
            Logger.Info("Opening file {0}", filename);

            if (_tabUtil.TryShowTab(filename, currentVar))
                return;

            if (!File.Exists(filename))
            {
                Logger.Warning("File could not be found: {0}, showing error message", filename);
                MessageBox.Show(string.Format(Resources.File_Could_Not_Be_Found, filename), Resources.Error, MessageBoxButtons.OK);
                return;
            }

            Cursor = Cursors.WaitCursor;

            try
            {
                _tabUtil.AddTab(filename);
                if (currentVar != null)
                    _tabUtil.CurrentFileControl.FindInCodeBox(currentVar.VariableName, false, false, false, true);
                Util.Properties.Settings.Default.RecentFiles.Add(filename);
                Util.Properties.Settings.Default.Save();
            }
            catch (Exception exception)
            {
                Logger.Error("Error opening file {0}: {1}", filename, exception.Message);
                MessageBox.Show(Resources.ErrorMessage_MainWindow_ErrorLoadingFile + exception.Message, Resources.Error, MessageBoxButtons.OK);
            }
            finally
            {
                Cursor = Cursors.Default;
            }
        }

        public void OpenFile()
        {
            openFileDialog.Filter = @"COBOL Files|*.cob;*.cbl;*.txt;.src";
            openFileDialog.FileName = "";

            var dialogResult = openFileDialog.ShowDialog();

            if (dialogResult == DialogResult.OK)
            {
                OpenFile(openFileDialog.FileName);
            }
        }

        public void Save()
        {
            if (_tabUtil.CurrentFileControl == null || _tabUtil.CurrentFileControl.CobolFile == null)
                return;

            if (_tabUtil.CurrentFileControl.HasFileReference())
                _tabUtil.CurrentFileControl.Save();
            else
                SaveAs();
        }

        public void SaveAs()
        {
            if (_tabUtil.CurrentFileControl == null || _tabUtil.CurrentFileControl.CobolFile == null)
                return;

            var currentFileRef = _tabUtil.CurrentFileControl.CobolFile.FileReference;
            if (currentFileRef != null)
            {
                saveFileDialog.InitialDirectory = currentFileRef.Directory;
                saveFileDialog.FileName = currentFileRef.ProgramName;
            }

            saveFileDialog.Filter = @"COBOL File|*.cob|COBOL Copy Book|*.cbl|Text File|*.txt";

            var dialogResult = saveFileDialog.ShowDialog();

            if (dialogResult == DialogResult.OK)
            {
                _tabUtil.CurrentFileControl.Save(saveFileDialog.FileName);
                _tabUtil.SetTabName(_tabUtil.CurrentFileControl.CobolFile.FileReference.ProgramName);
            }
        }

        public void Export()
        {
            if (_tabUtil.CurrentFileControl == null || _tabUtil.CurrentFileControl.CobolFile == null)
                return;

            var currentFileRef = _tabUtil.CurrentFileControl.CobolFile.FileReference;
            if (currentFileRef != null)
            {
                saveFileDialog.InitialDirectory = currentFileRef.Directory;
                saveFileDialog.FileName = currentFileRef.ProgramName + ".html";
            }

            saveFileDialog.Filter = @"HTML File|*.html";

            var dialogResult = saveFileDialog.ShowDialog();

            if (dialogResult == DialogResult.OK)
            {
                File.WriteAllText(saveFileDialog.FileName, _tabUtil.CurrentFileControl.ExportToHtml());
            }
        }

        public void CloseFile()
        {
            _tabUtil.CloseTab();
        }

        public void Exit()
        {
            if (_tabUtil.CloseAllTabs())
                Close();
        }

        #endregion

        #region Tools

        public void InsertCopybooksIntoSource()
        {
            if (_tabUtil.CurrentFileControl == null || _tabUtil.CurrentFileControl.CobolFile == null)
                return;

            _tabUtil.CurrentFileControl.InsertCopybooksIntoSource();
        }

        public void ShowLevel88Converter()
        {
            var converterWindow = new Level88ToEnum.Level88ToEnum();
            converterWindow.Show();
        }

        private void ShowCodeGenerator()
        {
            if (_tabUtil == null || _tabUtil.CurrentFileControl == null || _tabUtil.CurrentFileControl.CobolFile == null)
                return;

            var generator = new CodeGenerator.CodeGeneratorMainWindow(_tabUtil.CurrentFileControl.CobolFile);
            generator.Show(this);
        }

        #endregion

        #region Help

        public void ShowLog()
        {
            var logWindow = new Log();
            logWindow.Show();
        }

        public void ShowAbout()
        {
            var about = new About();
            about.Show();
        }

        public void ReportIssue(bool anonymously)
        {
            var url = anonymously
                ? "https://gitreports.com/issue/JannikArndt/Canal?name={0}&details={1}"
                : "https://github.com/JannikArndt/Canal/issues/new?body={1}";

            var body = "Log:" + Environment.NewLine + string.Join(Environment.NewLine, Logger.Singleton.GetEvents(LoggingLevel.Info, 20));

            url = string.Format(url, Environment.UserName, WebUtility.UrlEncode(body));
            Process.Start(url);
        }

        #endregion

        private void showFilesViewToolStripMenuItem_Click(object sender, EventArgs e)
        {
            splitContainer.Panel1Collapsed = !splitContainer.Panel1Collapsed;
        }
    }
}
