using System;
using System.Diagnostics;
using System.Windows.Forms;

namespace Canal
{
    public partial class MainWindow
    {
        private void OpenToolStripMenuItemClick(object sender, EventArgs e)
        {
            OpenFile();
        }

        private void CloseToolStripMenuItemClick(object sender, EventArgs e)
        {
            CloseFile();
        }

        private void Level88ToEnumConverterToolStripMenuItemClick(object sender, EventArgs e)
        {
            ShowLevel88Converter();
        }

        private void ExitToolStripMenuItemClick(object sender, EventArgs e)
        {
            Exit();
        }

        private void NewToolStripMenuItemClick(object sender, EventArgs e)
        {
            New();
        }

        private void NewProjectToolStripMenuItemClick(object sender, EventArgs e)
        {
            MessageBox.Show("Project files are not supported in this version of Canal.", "Not supported.",
                MessageBoxButtons.OK);
            // NewProject();
        }

        private void showFilesViewToolStripMenuItem_Click(object sender, EventArgs e)
        {
            splitContainer.Panel1Collapsed = !splitContainer.Panel1Collapsed;
        }

        private void ReportIssueToolStripMenuItemClick(object sender, EventArgs e)
        {
            ReportIssue(false);
        }

        private void reportIssueAnonymouslyToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ReportIssue(true);
        }

        private void AboutToolStripMenuItemClick(object sender, EventArgs e)
        {
            ShowAbout();
        }

        private void ShowLogToolStripMenuItemClick(object sender, EventArgs e)
        {
            ShowLog();
        }

        private void SaveToolStripMenuItemClick(object sender, EventArgs e)
        {
            Save();
        }

        private void SaveAsToolStripMenuItemClick(object sender, EventArgs e)
        {
            SaveAs();
        }

        private void ExportToolStripMenuItemClick(object sender, EventArgs e)
        {
            Export();
        }


        private void nextTabToolStripMenuItem_Click(object sender, EventArgs e)
        {
            _tabUtil.ShowNextTab();
        }

        private void previousTabToolStripMenuItem_Click(object sender, EventArgs e)
        {
            _tabUtil.ShowPreviousTab();
        }

        private void insertCopybooksIntoSourceToolStripMenuItem_Click(object sender, EventArgs e)
        {
            InsertCopybooksIntoSource();
        }

        private void showSourceOnGitHubToolStripMenuItem_Click(object sender, EventArgs e)
        {
            Process.Start("https://github.com/JannikArndt/Canal/");
        }

        private void reRunAnalysisToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_tabUtil == null || _tabUtil.CurrentFileControl == null)
                return;

            _tabUtil.CurrentFileControl.AnalyzeFile();
        }

        private void codeGeneratorToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ShowCodeGenerator();
        }
    }
}
