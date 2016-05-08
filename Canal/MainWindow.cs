namespace Canal
{
    using Level88ToEnum;
    using Logging;
    using Model;
    using Properties;
    using System;
    using System.Collections.Generic;
    using System.ComponentModel;
    using System.Diagnostics;
    using System.IO;
    using System.Windows.Forms;
    using UserControls;
    using Utils;

    public partial class MainWindow : Form
    {
        private readonly TabUtil _tabUtil;

        private readonly string[] _openFilesOnStartup;

        public MainWindow(string[] files = null)
        {
            InitializeComponent();

            Logger.Info("Starting program");

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
            return base.ProcessCmdKey(ref msg, keyData);
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
                Settings.Default.LastOpened.Add(filename);
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

        private void OpenToolStripMenuItemClick(object sender, EventArgs e)
        {
            openFileDialog.Filter = @"COBOL Files|*.cob;*.cbl;*.txt;.src";
            openFileDialog.FileName = "";

            var dialogResult = openFileDialog.ShowDialog();

            if (dialogResult == DialogResult.OK)
            {
                OpenFile(openFileDialog.FileName);
            }
        }

        private void CloseToolStripMenuItemClick(object sender, EventArgs e)
        {
            _tabUtil.CloseTab();
        }

        private void Level88ToEnumConverterToolStripMenuItemClick(object sender, EventArgs e)
        {
            var converterWindow = new Level88ToEnum();
            converterWindow.Show();
        }

        private void ExitToolStripMenuItemClick(object sender, EventArgs e)
        {
            if (_tabUtil.CloseAllTabs())
                Close();
        }

        private void NewToolStripMenuItemClick(object sender, EventArgs e)
        {
            _tabUtil.AddTab();
        }

        private void ReportIssueToolStripMenuItemClick(object sender, EventArgs e)
        {
            Process.Start("https://github.com/JannikArndt/Canal/issues/new");
        }

        private void AboutToolStripMenuItemClick(object sender, EventArgs e)
        {
            var about = new About();
            about.Show();
        }

        private void ShowLogToolStripMenuItemClick(object sender, EventArgs e)
        {
            var logWindow = new Log();
            logWindow.Show();
        }

        private void SaveToolStripMenuItemClick(object sender, EventArgs e)
        {
            if (_tabUtil.CurrentFileControl == null || _tabUtil.CurrentFileControl.CobolFile == null)
                return;

            if (_tabUtil.CurrentFileControl.HasFileReference())
                _tabUtil.CurrentFileControl.Save();
            else
                SaveAsToolStripMenuItemClick(sender, e);
        }

        private void SaveAsToolStripMenuItemClick(object sender, EventArgs e)
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

        private void ExportToolStripMenuItemClick(object sender, EventArgs e)
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

        private void nextTabToolStripMenuItem_Click(object sender, EventArgs e)
        {
            _tabUtil.ShowNextTab();
        }

        private void previousTabToolStripMenuItem_Click(object sender, EventArgs e)
        {
            _tabUtil.ShowPreviousTab();
        }
    }
}
