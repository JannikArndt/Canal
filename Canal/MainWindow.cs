﻿namespace Canal
{
    using System;
    using System.Collections.Generic;
    using System.ComponentModel;
    using System.Diagnostics;
    using System.IO;
    using System.Net;
    using System.Windows.Forms;
    using Logging;
    using Model.File;
    using Properties;
    using UserControls;
    using UserControls.VariableUsageAnalyzer;
    using Util;

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

            _tabUtil.SelectedTabChanged += UpdateMenuItems;
            _tabUtil.SelectedTabChanged += UpdateWindowName;
            _tabUtil.SavedVersionChanged += UpdateMenuItems;
            _tabUtil.FileSaved += UpdateMenuItems;
            UpdateMenuItems(null, null);

            LoadMostRecentDirectory();

            TextUtil.Instance.ErrorEventHandler += (sender, s) => MessageBox.Show(s, Resources.Error, MessageBoxButtons.OK);
        }

        private void LoadMostRecentDirectory()
        {
            FileUtil.Instance.AnalyzeFolder(MostRecentlyUsed.Instance.GetMostRecentFile());
        }

        private void UpdateWindowName(object sender, EventArgs e)
        {
            if (_tabUtil.CurrentFileControl != null)
            {
                Text = Resources.Main_Name_Short + @" - " + _tabUtil.CurrentFileControl.CobolFile.Name + @" (" +
                       _tabUtil.CurrentFileControl.CobolFile.FileReference.FilePath + @")";
            }
            else
            {
                Text = Resources.Main_Name_Long;
            }
        }

        private void UpdateMenuItems(object sender, EventArgs eventArgs)
        {
            var enabled = _tabUtil.CurrentFileControl != null && _tabUtil.CurrentFileControl.UnsavedChanges;

            saveToolStripMenuItem.Enabled = enabled;
            saveAsToolStripMenuItem.Enabled = enabled;
            revertChangesToolStripMenuItem.Enabled = enabled;
            exportToolStripMenuItem.Enabled = enabled;

            insertCopybooksIntoSourceToolStripMenuItem.Enabled = _tabUtil.CurrentFileControl != null;
            reRunAnalysisToolStripMenuItem.Enabled = _tabUtil.CurrentFileControl != null;
        }

        public void OpenVariableUsageWindow(Variable variable)
        {
            var variableUsageWindow = new VariableUsageAnalyzerMain(variable, _tabUtil.CurrentFileControl.CobolFile);
            variableUsageWindow.VariableUsageSelected += (sender, variable1, file, lineText) =>
            {
                OpenFile(file.FileReference.FilePath, variable1, lineText);
                Focus();
            };
            variableUsageWindow.Show();
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
                Logger.Error(exception, "Error trying to open files on startup {0}: {1}.",
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

        public void NewProject()
        {
            ProjectUtil.Instance.ShowProjectAssistant();
        }

        public void OpenFile(string filename, Variable currentVar = null, string lineText = null)
        {
            Logger.Info("Opening file {0}", filename);

            if (_tabUtil.TryShowTab(filename, currentVar, lineText))
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

                // Scrolling to right variable when the file is newly opened still has to be implemented like in TabUtil.TryShowTab
                if (currentVar != null)
                    _tabUtil.CurrentFileControl.FindInCodeBox(currentVar.VariableName, false, false, false, true);
                MostRecentlyUsed.Instance.Add(filename);
            }
            catch (Exception exception)
            {
                Logger.Error(exception, "Error opening file {0}: {1}", filename, exception.Message);
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

        public void SaveProject()
        {
            if (ProjectUtil.Instance.Current == null)
                return;

            if (string.IsNullOrWhiteSpace(ProjectUtil.Instance.CurrentFilename))
            {
                saveProjectDialog.InitialDirectory = ProjectUtil.Instance.Current.FilesRoot;
                saveProjectDialog.FileName = ProjectUtil.Instance.Current.Name;
            }

            var dialogResult = saveProjectDialog.ShowDialog();

            if (dialogResult == DialogResult.OK)
            {
                ProjectUtil.Instance.Save(saveProjectDialog.FileName);
            }
        }

        public void OpenProject()
        {
            if (ProjectUtil.Instance.Current != null)
            {
                var result = MessageBox.Show(Resources.Close_current_project_without_saving, Resources.Warning, MessageBoxButtons.YesNo);
                if (result == DialogResult.No)
                    return;
            }

            var dialogResult = openProjectDialog.ShowDialog();

            if (dialogResult == DialogResult.OK)
            {
                ProjectUtil.Instance.Open(openProjectDialog.FileName);
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

        public void RevertChanges()
        {
            if (_tabUtil.CurrentFileControl == null)
                return;

            var result = MessageBox.Show(Resources.RevertChanges_Text, Resources.RevertChanges_Title, MessageBoxButtons.OKCancel);

            if (result == DialogResult.OK)
                _tabUtil.CurrentFileControl.RevertChanges();
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
    }
}
