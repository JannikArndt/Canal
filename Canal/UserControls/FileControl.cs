using Canal.Events;
using Canal.Properties;
using Canal.Utils;
using FastColoredTextBoxNS.Events;
using Logging;
using Model;
using Model.References;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Linq;
using System.Text.RegularExpressions;
using System.Windows.Forms;

namespace Canal.UserControls
{
    public sealed partial class FileControl : UserControl, IDisposable
    {
        public CobolFile CobolFile { get; private set; }

        public event EventHandler<UsedFileTypesChangedEventArgs> UsedFileTypesChanged;

        public MainWindow MainWindow { get; private set; }

        private readonly BackgroundWorker _cobolTreeWorker = new BackgroundWorker();

        public FileControl(CobolFile file, MainWindow parent)
        {
            InitializeComponent();
            Name = "FileControl";
            Dock = DockStyle.Fill;

            try
            {
                MainWindow = parent;
                CobolFile = file;


                // initialize FastColoredTextBox
                codeBox.Font = SourceCodePro.Regular;
                codeBox.KeyDown += HandleKeyDown;
                codeBox.WordSelected += CodeBoxOnWordSelected;
                codeBox.SetTextAsync(CobolFile.Text);

                searchBox.Text = Resources.SearchPlaceholder;

                _cobolTreeWorker.DoWork += BuildCobolTree;
                // Display the analysis info in side tabs
                _cobolTreeWorker.RunWorkerCompleted += (sender, args) => InitTabs();
                // Build the CobolTree in the CobolFile which contains all analysis information
                _cobolTreeWorker.RunWorkerAsync(CobolFile);
            }
            catch (Exception exception)
            {
                Logger.Error("Error initializing file control for CobolFile {0}: {1}", file.Name, exception.Message);
                MessageBox.Show(Resources.ErrorMessage_FileControl_Constructor + exception.Message, Resources.Error, MessageBoxButtons.OK);
            }
        }

        public string GetText()
        {
            return codeBox.Text;
        }

        public string ExportToHtml()
        {
            return codeBox.Html;
        }

        private void InitTabs()
        {
            try
            {
                tocTabPage.Controls.Remove(loaderImageToc);
                splitContainerRight.Panel2.Controls.Remove(loaderImageInfoTab);

                ShowTocTreeView();

                ShowPerformsTreeView();

                ShowProceduresTreeView(true);

                ShowVariablesTreeView();

                ShowWordInfo();

                ShowFilesTreeView();
            }
            catch (Exception exception)
            {
                Logger.Error("Error initializing tabs for CobolFile {0}: {1}", CobolFile.Name, exception.Message);
                MessageBox.Show(Resources.ErrorMessage_FileControl_Constructor + exception.Message, Resources.Error, MessageBoxButtons.OK);
            }
        }

        private void ShowWordInfo(string word = "")
        {
            foreach (Control control in splitContainerRight.Panel2.Controls)
            {
                control.Dispose();
            }

            splitContainerRight.Panel2.Controls.Clear();
            var fileInfoControl = new WordInfo(word, CobolFile, this) { Dock = DockStyle.Fill };
            splitContainerRight.Panel2.Controls.Add(fileInfoControl);
        }

        private void ShowFilesTreeView()
        {
            FileUtil.Instance.ReduceDirectoriesToAllowedFiles();
            filesTreeView.Nodes.AddRange(FileUtil.Instance.GetDirectoryStructure());
            filesTreeView.ExpandAll();
            filesTabSearchBox.Text = Resources.SearchPlaceholder;
        }

        private void BuildCobolTree(object sender, DoWorkEventArgs doWorkEventArgs)
        {
            try
            {
                var builder = new CobolTreeBuilder();
                builder.Build(doWorkEventArgs.Argument as CobolFile);
            }
            catch (Exception exception)
            {
                MessageBox.Show(Resources.Error_CobolTree_could_not_be_built + exception.Message, Resources.Error, MessageBoxButtons.OK);
                throw;
            }
        }

        void IDisposable.Dispose()
        {
            _cobolTreeWorker.Dispose();
        }

        private void HandleKeyDown(object sender, KeyEventArgs e)
        {
            try
            {
                if (e.Shift)
                    switch (e.KeyCode)
                    {
                        case Keys.F3:
                            TrySearch(false, true);
                            return;
                    }
                else
                    switch (e.KeyCode)
                    {
                        case Keys.F3:
                            TrySearch(false);
                            return;
                        case Keys.Escape:
                            searchBox.Tag = false;
                            searchBox.Text = string.Empty;
                            searchBox.Tag = true;
                            return;
                    }
            }
            catch (Exception exception)
            {
                Logger.Error("Error processing key event {0} with KeyCode {1}: {2}.", e.ToString(), e.KeyCode, exception.Message);
            }
        }

        #region Code Box Events

        private void CodeBoxOnWordSelected(object sender, WordSelectedEventArgs eventArgs)
        {
            try
            {
                Logger.Info("Selected word {0}", eventArgs.Word);
                ShowWordInfo(eventArgs.Word);
            }
            catch (Exception exception)
            {
                Logger.Error("Error trying to show selected word {0}: {1}.", eventArgs.Word, exception.Message);
            }
        }

        #endregion

        #region Search Box

        private void TrySearch(bool firstSearch, bool reverse = false)
        {
            if (!(bool)searchBox.Tag) return;

            try
            {
                searchBox.BackColor = SystemColors.Window;
                codeBox.FindNext(searchBox.Text, false, searchWithRegEx.Checked, false, firstSearch, reverse);

            }
            catch (ArgumentException)
            {
                // aparently the only way to validate a regex
                searchBox.BackColor = Color.PaleVioletRed;
            }
            catch (Exception exception)
            {
                Logger.Error("Error trying to search for {0}: {1}.", searchBox.Text, exception.GetType());
            }
        }

        private void seachBox_TextChanged(object sender, EventArgs e)
        {
            TrySearch(true);
        }

        private void searchBox_Enter(object sender, EventArgs e)
        {
            var box = (ToolStripTextBox)sender;
            if (box.Text == Resources.SearchPlaceholder)
            {
                box.Tag = false;
                box.Text = "";
                box.Tag = true;
            }
        }

        private void searchBox_Leave(object sender, EventArgs e)
        {
            var box = (ToolStripTextBox)sender;
            if (string.IsNullOrWhiteSpace(box.Text))
            {
                box.Tag = false;
                box.Text = Resources.SearchPlaceholder;
                box.Tag = true;
            }
        }

        #endregion

        #region Tree View Selects

        private void treeView_AfterSelect(object sender, TreeViewEventArgs e)
        {
            codeBox.FindNext(@"^.{7}" + tocTreeView.SelectedNode.Text + @"(\.| +USING| OF)", false, true, false, true);
        }

        private void performsTree_AfterSelect(object sender, TreeViewEventArgs e)
        {
            var treeNode = performsTreeView.SelectedNode;
            if (treeNode == null)
                codeBox.DoRangeVisible(codeBox.GetRange(0, 0));
            else
                codeBox.FindNext(@"^.{7}" + treeNode.Text + @"(\.| +USING)", false, true, false, true);
        }

        private void variablesTreeView_AfterSelect(object sender, TreeViewEventArgs e)
        {
            var treeNode = variablesTreeView.SelectedNode as Variable;
            if (treeNode == null)
                codeBox.DoRangeVisible(codeBox.GetRange(0, 0));
            else
                codeBox.FindNext(treeNode.Text, false, false, false, true);
        }

        private void proceduresTreeView_AfterSelect(object sender, TreeViewEventArgs e)
        {
            var treeNode = proceduresTreeView.SelectedNode;
            if (treeNode == null)
                codeBox.DoRangeVisible(codeBox.GetRange(0, 0));
            else if (!Regex.IsMatch(treeNode.Text, @"^\d\d"))
                codeBox.FindNext(@"^.{7}" + treeNode.Text + @"(\.| +USING)", false, true, false, true);
        }

        public void FindInCodeBox(string pattern, bool matchCase, bool regex, bool wholeWord, bool firstSearch = false)
        {
            codeBox.FindNext(pattern, matchCase, regex, wholeWord, firstSearch);
        }

        #endregion

        #region Button Clicks

        private void ResolveCopysButton_Click(object sender, EventArgs e)
        {
            ResolveCopysButton.Enabled = false;

            var backgroundWorker = new BackgroundWorker();

            backgroundWorker.DoWork += (o, args) =>
            {
                var refUtil = new ReferenceUtil();
                refUtil.ProgressChanged += (sender1, eventArgs) => backgroundWorker.ReportProgress(eventArgs.ProgressPercentage);
                refUtil.ResolveCopys(CobolFile);
                args.Result = CobolFile;
            };

            backgroundWorker.RunWorkerCompleted += (o, args) =>
            {
                CobolFile = args.Result as CobolFile;
                if (CobolFile == null) return;

                codeBox.SetTextAsync(CobolFile.Text);

                toolStripProgressBar.Value = 100;

                toolStripProgressBar.Visible = false;

                // Build the CobolTree in the CobolFile which contains all analysis information
                _cobolTreeWorker.RunWorkerAsync(CobolFile);
            };

            Cursor = Cursors.WaitCursor;

            toolStripProgressBar.Visible = true;

            backgroundWorker.WorkerReportsProgress = true;
            backgroundWorker.ProgressChanged += (o, args) => toolStripProgressBar.Value = args.ProgressPercentage;
            backgroundWorker.RunWorkerAsync();

            Cursor = Cursors.Default;
        }

        private void ExportTocClick(object sender, EventArgs e)
        {
            Clipboard.SetText(tocTreeView.ToText());
        }

        private void CopyProceduresClick(object sender, EventArgs e)
        {
            Clipboard.SetText(proceduresTreeView.ToText());
        }

        #endregion

        #region Tree View Initializers

        private void ShowVariablesTreeView()
        {
            variablesTreeView.Nodes.Clear();

            if (CobolFile.CobolTree == null)
            {
                variablesTreeView.Nodes.Add(new TreeNode("Error: CobolTree could not be built.") { ForeColor = Color.Red });
                return;
            }

            var workingStorageSectionTreeNode = new TreeNode("Working-Storage Division");

            foreach (var variable in CobolFile.CobolTree.DataDivision.WorkingStorageSection.Variables)
            {
                variable.FillNodesWithVariables();
                workingStorageSectionTreeNode.Nodes.Add(variable);
            }

            var linkageSectionTreeNode = new TreeNode("Linkage Division");

            foreach (var variable in CobolFile.CobolTree.DataDivision.LinkageSection.Variables)
            {
                variable.FillNodesWithVariables();
                linkageSectionTreeNode.Nodes.Add(variable);
            }

            variablesTreeView.Nodes.Add(workingStorageSectionTreeNode);
            variablesTreeView.Nodes.Add(linkageSectionTreeNode);
        }

        private void ShowTocTreeView()
        {
            tocTreeView.Nodes.Clear();

            if (CobolFile.CobolTree == null)
            {
                tocTreeView.Nodes.Add(new TreeNode("Error: CobolTree could not be built.") { ForeColor = Color.Red });
                return;
            }

            tocTreeView.Nodes.Add(CobolFile.CobolTree.GetAsTreeNodes());
            tocTreeView.ExpandAll();
        }

        private void ShowPerformsTreeView()
        {
            performsTreeView.Nodes.Clear();

            if (CobolFile.CobolTree == null)
            {
                performsTreeView.Nodes.Add(new TreeNode("Error: CobolTree could not be built.") { ForeColor = Color.Red });
                return;
            }

            var performTreeWorker = new BackgroundWorker();

            performTreeWorker.DoWork += delegate (object sender, DoWorkEventArgs args)
            {
                args.Result = ReferenceUtil.GetPerformTree(CobolFile);
            };

            performTreeWorker.RunWorkerCompleted += delegate (object sender, RunWorkerCompletedEventArgs args)
            {
                performsTabPage.Controls.Remove(loaderImagePerforms);
                performsTreeView.Nodes.Add((TreeNode)args.Result);
            };

            performTreeWorker.RunWorkerAsync();

        }

        private void ShowProceduresTreeView(bool showWarning = false)
        {
            proceduresTreeView.Nodes.Clear();

            if (CobolFile.CobolTree == null)
            {
                proceduresTreeView.Nodes.Add(new TreeNode("Error: CobolTree could not be built.") { ForeColor = Color.Red });
                return;
            }

            if (showWarning)
                proceduresTreeView.Nodes.Add(new TreeNode("Copy files aren't resolved yet, references may be incomplete!") { ForeColor = Color.Red });

            foreach (var section in CobolFile.CobolTree.ProcedureDivision.Sections)
            {
                var sectionNode = new TreeNode(section.Name);

                foreach (var procedure in section.Procedures)
                {
                    var procNode = new TreeNode(procedure.Name);
                    sectionNode.Nodes.Add(procNode);

                    var varDict = new Dictionary<Variable, List<Variable>>();

                    foreach (var variable in procedure.Variables.Keys)
                    {
                        var root = variable.Root ?? variable;
                        if (varDict.ContainsKey(root))
                            varDict[root].Add(variable);
                        else
                            varDict.Add(root, new List<Variable> { variable });
                    }

                    foreach (var key in varDict.Keys.OrderBy(r => r.VariableName))
                    {
                        var rootVarNode = new TreeNode(key.VariableName);
                        foreach (var variable in varDict[key])
                            rootVarNode.Nodes.Add(new TreeNode(variable.VariableLevel.ToString("D2") + "  " + variable.VariableName + " " + procedure.Variables[variable].ToShortString()));

                        procNode.Nodes.Add(rootVarNode);
                    }
                }

                proceduresTreeView.Nodes.Add(sectionNode);
            }
        }

        #endregion

        #region Expand and Collapse Buttons

        private void TocExpandAllButton_Click(object sender, EventArgs e)
        {
            tocTreeView.ExpandAll();
        }

        private void TocCollapseAllButton_Click(object sender, EventArgs e)
        {
            tocTreeView.CollapseAll();
        }

        private void proceduresExpandAllButton_Click(object sender, EventArgs e)
        {
            proceduresTreeView.ExpandAll();
        }

        private void proceduresCollapseAllButton_Click(object sender, EventArgs e)
        {
            proceduresTreeView.CollapseAll();
        }

        private void variablesCopyButton_Click(object sender, EventArgs e)
        {
            Clipboard.SetText(variablesTreeView.ToText());
        }

        private void variablesExpandAllButton_Click(object sender, EventArgs e)
        {
            variablesTreeView.ExpandAll();
        }

        private void variablesCollapseAllButton_Click(object sender, EventArgs e)
        {
            variablesTreeView.CollapseAll();
        }

        private void performsCopyButton_Click(object sender, EventArgs e)
        {
            Clipboard.SetText(proceduresTreeView.ToText());
        }

        private void performsExpandAllButton_Click(object sender, EventArgs e)
        {
            performsTreeView.ExpandAll();
        }

        private void performsCollapseAllButton_Click(object sender, EventArgs e)
        {
            performsTreeView.CollapseAll();
        }

        #endregion

        #region Files Tree

        private void filesTabSearchBox_TextChanged(object sender, EventArgs e)
        {
            if (((ToolStripTextBox)sender).Text == Resources.SearchPlaceholder)
                return;
            filesTreeView.Nodes.Clear();
            filesTreeView.Nodes.AddRange(FileUtil.Instance.GetDirectoryStructure(((ToolStripTextBox)sender).Text));
            filesTreeView.ExpandAll();

            if (filesTreeView.Nodes.Count == 1 && filesTreeView.Nodes[0].Nodes.Count == 1)
            {
                filesTreeView.SelectedNode = filesTreeView.Nodes[0].Nodes[0];
                filesTreeView.Focus();
            }
        }

        private void filesTreeView_DoubleClick(object sender, EventArgs e)
        {
            if (filesTreeView.SelectedNode == null || filesTreeView.SelectedNode.Tag == null)
                return;

            MainWindow.OpenFile(((FileReference)filesTreeView.SelectedNode.Tag).FilePath);
        }

        private void filesTreeView_KeyUp(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter)
                MainWindow.OpenFile(((FileReference)filesTreeView.SelectedNode.Tag).FilePath);
        }

        private void settings_sourceCodeFiles_Click(object sender, EventArgs e)
        {
            Settings.Default.FileTypeCob = showFileTypes_cob.Checked;
            Settings.Default.FileTypeTxt = showFileTypes_txt.Checked;
            Settings.Default.FileTypeSrc = showFileTypes_src.Checked;
            Settings.Default.FileTypeCustom = showFileTypes_custom.Text;
            Settings.Default.Save();

            if (UsedFileTypesChanged != null) UsedFileTypesChanged(this, new UsedFileTypesChangedEventArgs());

            RefreshFileView();
        }

        /// <summary>
        /// Updates the DropDownButton in the Files-tab with the settings stored in Settings.Default.FileType* and refreshes the Files-tab
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        public void RefreshUsedFileTypes(object sender, EventArgs e)
        {
            showFileTypes_cob.Checked = Settings.Default.FileTypeCob;
            showFileTypes_txt.Checked = Settings.Default.FileTypeTxt;
            showFileTypes_src.Checked = Settings.Default.FileTypeSrc;
            showFileTypes_custom.Text = Settings.Default.FileTypeCustom;

            RefreshFileView();
        }

        /// <summary>
        /// Refreshes the Files-tab
        /// </summary>
        public void RefreshFileView()
        {
            var searchText = filesTabSearchBox.Text == Resources.SearchPlaceholder ? "" : filesTabSearchBox.Text;
            FileUtil.Instance.ReduceDirectoriesToAllowedFiles();
            var nodes = FileUtil.Instance.GetDirectoryStructure(searchText);
            filesTreeView.Nodes.Clear();
            filesTreeView.Nodes.AddRange(nodes);
            filesTreeView.ExpandAll();
        }

        #endregion

        private void navigateBackward_Click(object sender, EventArgs e)
        {
            codeBox.NavigateBackward();
            navigateForwardButton.Enabled = true;
        }

        private void navigateForwardButton_Click(object sender, EventArgs e)
        {
            var success = codeBox.NavigateForward();
            if (!success)
                navigateForwardButton.Enabled = false;
        }

        private void findNextButton_Click(object sender, EventArgs e)
        {
            TrySearch(false);
        }

        private void findPreviousButton_Click(object sender, EventArgs e)
        {
            TrySearch(false, true);
        }
    }
}
