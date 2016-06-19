using Canal.Properties;
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
                MessageBox.Show("Please enter a valid project name.", Resources.Error, MessageBoxButtons.OK);
                return;
            }

            if (string.IsNullOrWhiteSpace(FolderTextBox.Text))
            {
                MessageBox.Show("Please enter a valid filepath.", Resources.Error, MessageBoxButtons.OK);
                return;
            }

            if (FileTypesCheckedListBox.CheckedItems.Count == 0)
            {
                MessageBox.Show("Please select at least one file type.", Resources.Error, MessageBoxButtons.OK);
                return;
            }

            var project = new CobolProject(NameTextBox.Text, FolderTextBox.Text, new List<string>(FileTypesCheckedListBox.CheckedItems.Cast<string>()));

            var worker = new BackgroundWorker();
            worker.DoWork += ProjectUtil.Instance.CreateProject;
            worker.ProgressChanged += (o, args) => ProgressBar.Increment(1);
            worker.RunWorkerCompleted += (o, args) =>
            {
                Success = true;
                Close();
            };
            worker.RunWorkerAsync(project);
        }

        private void FolderTextBox_TextChanged(object sender, EventArgs e)
        {
            UpdateFileCountAsync(FolderTextBox.Text);
        }

        private void SelectFolder(object sender, EventArgs e)
        {
            if (string.IsNullOrWhiteSpace(folderBrowserDialog.SelectedPath))
            {
                var recentFiles = Util.Properties.Settings.Default.RecentFiles;
                var mostRecentFolder = recentFiles[recentFiles.Count - 1];
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

            var folderWorker = new BackgroundWorker();
            folderWorker.DoWork += (o, args) => FileUtil.Instance.AnalyzeFolder(dir);
            folderWorker.RunWorkerCompleted +=
                (o, args) => ProgressLabel.Text = FileUtil.Instance.CountValidFiles(dir) + @" Files found.";

            folderWorker.RunWorkerAsync();
        }
    }
}
