using Canal.Properties;
using Logging;
using Model.Project;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Windows.Forms;
using Util;

namespace Canal.UserControls
{
    public partial class ProjectAssistant : Form
    {
        public bool Success { get; set; }

        public bool Cancelled { get; set; }

        public CobolProject Result { get; private set; }

        private readonly BackgroundWorker _folderWorker = new BackgroundWorker();

        private readonly BackgroundWorker _analysisWorker = new BackgroundWorker();

        public ProjectAssistant()
        {
            InitializeComponent();

            FileTypesCheckedListBox.SetItemChecked(0, Util.Properties.Settings.Default.FileTypeCob);
            FileTypesCheckedListBox.SetItemChecked(1, Util.Properties.Settings.Default.FileTypeTxt);
            FileTypesCheckedListBox.SetItemChecked(2, Util.Properties.Settings.Default.FileTypeSrc);
        }

        protected override bool ProcessCmdKey(ref Message msg, Keys keyData)
        {
            if (keyData == Keys.Escape)
                Close();

            if (keyData == Keys.Enter)
                StartAnalysisButton_Click(this, null);

            return base.ProcessCmdKey(ref msg, keyData);
        }

        private void StartAnalysisButton_Click(object sender, EventArgs e)
        {
            if (string.IsNullOrWhiteSpace(NameTextBox.Text))
            {
                MessageBox.Show(Resources.ProjectAssistant_StartAnalysisButton_Click_Please_enter_a_valid_project_name_, Resources.Error, MessageBoxButtons.OK);
                return;
            }

            if (string.IsNullOrWhiteSpace(FolderTextBox.Text) || !Directory.Exists(FolderTextBox.Text))
            {
                MessageBox.Show(Resources.ProjectAssistant_StartAnalysisButton_Click_Please_enter_a_valid_filepath_, Resources.Error, MessageBoxButtons.OK);
                return;
            }

            if (FileTypesCheckedListBox.CheckedItems.Count == 0)
            {
                MessageBox.Show(Resources.ProjectAssistant_StartAnalysisButton_Click_Please_select_at_least_one_file_type_, Resources.Error, MessageBoxButtons.OK);
                return;
            }

            try
            {
                ProgressBar.Visible = true;

                var project = new CobolProject(NameTextBox.Text, FolderTextBox.Text, new List<string>(FileTypesCheckedListBox.CheckedItems.Cast<string>()));

                _analysisWorker.DoWork += ProjectUtil.Instance.LoadFiles;
                _analysisWorker.WorkerReportsProgress = true;
                _analysisWorker.ProgressChanged += (o, args) => ProgressBar.Value = args.ProgressPercentage;
                _analysisWorker.RunWorkerCompleted += (o, args) =>
                {
                    if (Cancelled)
                        return;

                    ProgressBar.Visible = false;
                    Result = args.Result as CobolProject;
                    Success = Result != null;
                    Close();
                };
                _analysisWorker.RunWorkerAsync(project);
            }
            catch (Exception exception)
            {
                Logger.Error(exception, "Project creation failed: {0}.", exception.Message);
                MessageBox.Show(Resources.ProjectAssistant_StartAnalysisButton_Click_Project_creation_failed__ + exception.Message, Resources.Error, MessageBoxButtons.OK);
            }
        }

        private void FolderTextBox_TextChanged(object sender, EventArgs e)
        {
            UpdateFileCountAsync(FolderTextBox.Text);
        }

        private void SelectFolder(object sender, EventArgs e)
        {
            if (string.IsNullOrWhiteSpace(folderBrowserDialog.SelectedPath))
            {
                var mostRecentFolder = MostRecentlyUsed.Instance.GetMostRecentFile();
                folderBrowserDialog.SelectedPath = Path.GetDirectoryName(mostRecentFolder);
            }

            var result = folderBrowserDialog.ShowDialog();
            if (result == DialogResult.OK)
                FolderTextBox.Text = folderBrowserDialog.SelectedPath;
        }

        private void FileTypesCheckedListBox_ItemCheck(object sender, MouseEventArgs e)
        {
            SaveUsedFileTypesToSettings();

            if (!string.IsNullOrWhiteSpace(FolderTextBox.Text))
                UpdateFileCountAsync(FolderTextBox.Text);
        }

        private void SaveUsedFileTypesToSettings()
        {
            Util.Properties.Settings.Default.FileTypeCob = FileTypesCheckedListBox.GetItemChecked(0);
            Util.Properties.Settings.Default.FileTypeTxt = FileTypesCheckedListBox.GetItemChecked(1);
            Util.Properties.Settings.Default.FileTypeSrc = FileTypesCheckedListBox.GetItemChecked(2);
            Util.Properties.Settings.Default.Save();
        }

        private void UpdateFileCountAsync(string dir)
        {
            ProgressLabel.Text = "";

            if (!File.Exists(dir))
                return;

            _folderWorker.DoWork += (o, args) => FileUtil.Instance.AnalyzeFolder(dir);
            _folderWorker.RunWorkerCompleted +=
                (o, args) =>
                {
                    if (!Cancelled) ProgressLabel.Text = FileUtil.Instance.CountValidFiles(dir) + @" Files found.";
                };

            _folderWorker.RunWorkerAsync();
        }

        private void CancelButton1_Click(object sender, EventArgs e)
        {
            if (_analysisWorker.WorkerSupportsCancellation)
                _analysisWorker.CancelAsync();

            if (_folderWorker.WorkerSupportsCancellation)
                _folderWorker.CancelAsync();

            Cancelled = true;
            Close();
        }
    }
}
