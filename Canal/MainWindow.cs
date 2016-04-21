namespace Canal
{
    using System;
    using System.Collections.Generic;
    using System.ComponentModel;
    using System.Diagnostics;
    using System.IO;
    using System.Windows.Forms;

    using Canal.Properties;
    using Canal.UserControls;
    using Canal.Utils;

    using Level88ToEnum;

    using Logging;

    using Model;
    using Model.References;

    public partial class MainWindow : Form
    {
        private readonly TabUtil tabUtil;

        private readonly string[] openFilesOnStartup;

        public MainWindow(string[] files = null)
        {
            InitializeComponent();

            Logger.Info("Starting program");

            this.openFilesOnStartup = files;

            this.tabUtil = new TabUtil(FileTabs, this);
            this.tabUtil.ShowStartTab();
        }

        protected override void OnShown(EventArgs e)
        {
            try
            {
                var toOpen = new List<string>();
                if (this.openFilesOnStartup != null) toOpen.AddRange(this.openFilesOnStartup);

                foreach (string filepath in new HashSet<string>(toOpen))
                    OpenFile(filepath);
            }
            catch (Exception exception)
            {
                Logger.Error("Error trying to open files on startup {0}: {1}.",
                    this.openFilesOnStartup == null ? "_openFilesOnStartup is null" : string.Join(", ", this.openFilesOnStartup),
                    exception.Message);
                MessageBox.Show(Resources.ErrorMessage_MainWindow_OpenPrevious + exception.Message, Resources.Error, MessageBoxButtons.OK);
            }
        }

        protected override void OnClosing(CancelEventArgs e)
        {
            Logger.Info("Closing program");
            Settings.Default.Save();
            base.OnClosing(e);
        }

        public void OpenFile(string filename)
        {
            Logger.Info("Opening file {0}", filename);

            if (this.tabUtil.TryShowTab(filename))
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
                var file = FileUtil.Instance.Get(filename);
                this.tabUtil.AddTab(file);
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
            this.tabUtil.CloseTab();
        }

        private void Level88ToEnumConverterToolStripMenuItemClick(object sender, EventArgs e)
        {
            var converterWindow = new Level88ToEnum();
            converterWindow.Show();
        }

        private void ExitToolStripMenuItemClick(object sender, EventArgs e)
        {
            if (this.tabUtil.CloseAllTabs())
                Close();
        }

        private void NewToolStripMenuItemClick(object sender, EventArgs e)
        {
            this.tabUtil.AddTab(new CobolFile("", "New File"));
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
            if (this.tabUtil.CurrentFileControl == null || this.tabUtil.CurrentFileControl.CobolFile == null)
                return;

            var currentFileRef = this.tabUtil.CurrentFileControl.CobolFile.FileReference;
            if (currentFileRef == null)
            {
                this.SaveAsToolStripMenuItemClick(sender, e);
                return;
            }

            File.WriteAllText(currentFileRef.FilePath, this.tabUtil.CurrentFileControl.GetText());
        }

        private void SaveAsToolStripMenuItemClick(object sender, EventArgs e)
        {
            if (this.tabUtil.CurrentFileControl == null || this.tabUtil.CurrentFileControl.CobolFile == null)
                return;

            var currentFileRef = this.tabUtil.CurrentFileControl.CobolFile.FileReference;
            if (currentFileRef != null)
            {
                saveFileDialog.InitialDirectory = currentFileRef.Directory;
                saveFileDialog.FileName = currentFileRef.ProgramName;
            }

            saveFileDialog.Filter = @"COBOL File|*.cob|COBOL Copy Book|*.cbl|Text File|*.txt";

            var dialogResult = saveFileDialog.ShowDialog();

            if (dialogResult == DialogResult.OK)
            {
                File.WriteAllText(saveFileDialog.FileName, this.tabUtil.CurrentFileControl.GetText());
                this.tabUtil.CurrentFileControl.CobolFile.FileReference = new FileReference(saveFileDialog.FileName);
                this.tabUtil.SetTabName(this.tabUtil.CurrentFileControl.CobolFile.FileReference.ProgramName);
            }
        }

        private void ExportToolStripMenuItemClick(object sender, EventArgs e)
        {
            if (this.tabUtil.CurrentFileControl == null || this.tabUtil.CurrentFileControl.CobolFile == null)
                return;

            var currentFileRef = this.tabUtil.CurrentFileControl.CobolFile.FileReference;
            if (currentFileRef != null)
            {
                saveFileDialog.InitialDirectory = currentFileRef.Directory;
                saveFileDialog.FileName = currentFileRef.ProgramName + ".html";
            }

            saveFileDialog.Filter = @"HTML File|*.html";

            var dialogResult = saveFileDialog.ShowDialog();

            if (dialogResult == DialogResult.OK)
            {
                File.WriteAllText(saveFileDialog.FileName, this.tabUtil.CurrentFileControl.ExportToHtml());
            }
        }
    }
}
