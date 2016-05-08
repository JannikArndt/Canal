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
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Windows.Forms;

namespace Canal.UserControls
{
    public sealed partial class FileControl : UserControl
    {
        public FileControl(string filename, MainWindow parent)
        {
            InitializeComponent();
            Name = "FileControl";
            Dock = DockStyle.Fill;

            try
            {
                MainWindow = parent;

                CobolFile = new CobolFileBuilder().Build(filename);

                var worker = new BackgroundWorker();
                worker.DoWork += new Analyzer().AnalyzeFile;
                worker.RunWorkerCompleted += InitTabs;
                worker.RunWorkerAsync(CobolFile);

                // initialize FastColoredTextBox
                codeBox.Font = SourceCodePro.Regular;
                codeBox.SetTextAsync(CobolFile.Text);

                codeBox.KeyDown += HandleKeyDown;
                codeBox.WordSelected += CodeBoxOnWordSelected;
                codeBox.TextChanged += (sender, args) =>
                {
                    if (!UnsavedChanges && SavedVersionChanged != null)
                        SavedVersionChanged(sender, args);
                    UnsavedChanges = true;
                };

                searchBox.Text = Resources.SearchPlaceholder;
            }
            catch (Exception exception)
            {
                Logger.Error("Error initializing file control for CobolFile {0}: {1}", filename, exception.Message);
                MessageBox.Show(Resources.ErrorMessage_FileControl_Constructor + exception.Message, Resources.Error, MessageBoxButtons.OK);
            }
        }

        public event EventHandler<UsedFileTypesChangedEventArgs> UsedFileTypesChanged;

        /// <summary>
        /// Event is thrown the first time the text is changed after it is loaded or saved.
        /// </summary>
        public event EventHandler<TextChangedEventArgs> SavedVersionChanged;

        public event EventHandler<FileSystemEventArgs> FileSaved;

        /// <summary>
        /// Shows if there are unsaved changes in the text.
        /// </summary>
        public bool UnsavedChanges { get; private set; }

        public CobolFile CobolFile { get; private set; }

        public MainWindow MainWindow { get; private set; }

        public bool HasFileReference()
        {
            return CobolFile.FileReference != null;
        }

        public string ExportToHtml()
        {
            return codeBox.Html;
        }

        public void Save(string filename = "")
        {
            if (!string.IsNullOrWhiteSpace(filename))
                CobolFile.FileReference = new FileReference(filename);

            File.WriteAllText(CobolFile.FileReference.FilePath, codeBox.Text);
            UnsavedChanges = false;
            if (FileSaved != null)
                FileSaved(this, new FileSystemEventArgs(WatcherChangeTypes.Changed, CobolFile.FileReference.Directory, CobolFile.FileReference.ProgramName));
        }

        public void FindInCodeBox(string pattern, bool matchCase, bool regex, bool wholeWord, bool firstSearch = false)
        {
            codeBox.FindNext(pattern, matchCase, regex, wholeWord, firstSearch);
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

        private void InitTabs(object sender, RunWorkerCompletedEventArgs runWorkerCompletedEventArgs)
        {
            try
            {
                tocTabPage.Controls.Remove(loaderImageToc);
                splitContainerRight.Panel2.Controls.Remove(loaderImageInfoTab);

                ShowTocTreeView();

                ShowPerformsTreeView();

                ShowProceduresTreeView();

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
            if (IsDisposed) return;

            FileUtil.Instance.ReduceDirectoriesToAllowedFiles();
            filesTreeView.Nodes.AddRange(FileUtil.Instance.GetDirectoryStructure());
            filesTreeView.ExpandAll();
            filesTabSearchBox.Text = Resources.SearchPlaceholder;
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

        private void SeachBoxTextChanged(object sender, EventArgs e)
        {
            TrySearch(true);
        }

        private void SearchBoxEnter(object sender, EventArgs e)
        {
            var box = (ToolStripTextBox)sender;
            if (box.Text == Resources.SearchPlaceholder)
            {
                box.Tag = false;
                box.Text = "";
                box.Tag = true;
            }
        }

        private void SearchBoxLeave(object sender, EventArgs e)
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

        private void TreeViewAfterSelect(object sender, TreeViewEventArgs e)
        {
            codeBox.FindNext(@"^.{7}" + tocTreeView.SelectedNode.Text + @"(\.| +USING| OF)", false, true, false, true);
        }

        private void PerformsTreeAfterSelect(object sender, TreeViewEventArgs e)
        {
            var treeNode = performsTreeView.SelectedNode;
            if (treeNode == null)
                codeBox.DoRangeVisible(codeBox.GetRange(0, 0));
            else
                codeBox.FindNext(@"^.{7}" + treeNode.Text + @"(\.| +USING)", false, true, false, true);
        }

        private void VariablesTreeViewAfterSelect(object sender, TreeViewEventArgs e)
        {
            var treeNode = variablesTreeView.SelectedNode as Variable;
            if (treeNode == null)
                codeBox.DoRangeVisible(codeBox.GetRange(0, 0));
            else
                codeBox.FindNext(treeNode.Text, false, false, false, true);
        }

        private void ProceduresTreeViewAfterSelect(object sender, TreeViewEventArgs e)
        {
            var treeNode = proceduresTreeView.SelectedNode;
            if (treeNode == null)
                codeBox.DoRangeVisible(codeBox.GetRange(0, 0));
            else if (!Regex.IsMatch(treeNode.Text, @"^\d\d"))
                codeBox.FindNext(@"^.{7}" + treeNode.Text + @"(\.| +USING)", false, true, false, true);
        }

        #endregion

        #region Button Clicks

        private void ResolveCopysButtonClick(object sender, EventArgs e)
        {
            ResolveCopysButton.Enabled = false;

            CobolFile.Text = codeBox.Text;

            // TODO

            var backgroundWorker = new BackgroundWorker();

            backgroundWorker.DoWork += (o, args) =>
            {
                ReferenceUtil.Instance.ProgressChanged += (sender1, eventArgs) => backgroundWorker.ReportProgress(eventArgs.ProgressPercentage);
                ReferenceUtil.Instance.ResolveCopys(CobolFile);
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
                // _cobolTreeWorker.RunWorkerAsync(CobolFile);
            };

            Cursor = Cursors.WaitCursor;

            toolStripProgressBar.Visible = true;

            backgroundWorker.WorkerReportsProgress = true;
            backgroundWorker.ProgressChanged += (o, args) => toolStripProgressBar.Value = args.ProgressPercentage;
            backgroundWorker.RunWorkerAsync();

            ResolveCopysButton.Enabled = true;

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

            var workingStorageSectionTreeNode = new TreeNode("Local");

            foreach (var variable in CobolFile.Variables.Values.Where(vari => vari.VariableLevel == 1 && vari.CopyReference.CobolFile == CobolFile))
            {
                variable.FillNodesWithVariables();
                workingStorageSectionTreeNode.Nodes.Add(variable);
            }

            var linkageSectionTreeNode = new TreeNode("From Records");

            foreach (var variable in CobolFile.Variables.Values.Where(vari => vari.VariableLevel == 1 && vari.CopyReference.CobolFile != CobolFile))
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
                args.Result = ReferenceUtil.Instance.GetPerformTree(CobolFile);
            };

            performTreeWorker.RunWorkerCompleted += delegate (object sender, RunWorkerCompletedEventArgs args)
            {
                performsTabPage.Controls.Remove(loaderImagePerforms);
                performsTreeView.Nodes.Add((TreeNode)args.Result);
            };

            performTreeWorker.RunWorkerAsync();
        }

        private void ShowProceduresTreeView()
        {
            proceduresTreeView.Nodes.Clear();

            if (CobolFile.CobolTree == null)
            {
                proceduresTreeView.Nodes.Add(new TreeNode("Error: CobolTree could not be built.") { ForeColor = Color.Red });
                return;
            }

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

        private void TocExpandAllButtonClick(object sender, EventArgs e)
        {
            tocTreeView.ExpandAll();
        }

        private void TocCollapseAllButtonClick(object sender, EventArgs e)
        {
            tocTreeView.CollapseAll();
        }

        private void ProceduresExpandAllButtonClick(object sender, EventArgs e)
        {
            proceduresTreeView.ExpandAll();
        }

        private void ProceduresCollapseAllButtonClick(object sender, EventArgs e)
        {
            proceduresTreeView.CollapseAll();
        }

        private void VariablesCopyButtonClick(object sender, EventArgs e)
        {
            Clipboard.SetText(variablesTreeView.ToText());
        }

        private void VariablesExpandAllButtonClick(object sender, EventArgs e)
        {
            variablesTreeView.ExpandAll();
        }

        private void VariablesCollapseAllButtonClick(object sender, EventArgs e)
        {
            variablesTreeView.CollapseAll();
        }

        private void PerformsCopyButtonClick(object sender, EventArgs e)
        {
            Clipboard.SetText(proceduresTreeView.ToText());
        }

        private void PerformsExpandAllButtonClick(object sender, EventArgs e)
        {
            performsTreeView.ExpandAll();
        }

        private void PerformsCollapseAllButtonClick(object sender, EventArgs e)
        {
            performsTreeView.CollapseAll();
        }

        #endregion

        #region Files Tree

        private void FilesTabSearchBoxTextChanged(object sender, EventArgs e)
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

        private void FilesTreeViewDoubleClick(object sender, EventArgs e)
        {
            if (filesTreeView.SelectedNode == null || filesTreeView.SelectedNode.Tag == null)
                return;

            MainWindow.OpenFile(((FileReference)filesTreeView.SelectedNode.Tag).FilePath);
        }

        private void FilesTreeViewKeyUp(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter)
                MainWindow.OpenFile(((FileReference)filesTreeView.SelectedNode.Tag).FilePath);
        }

        private void SettingsSourceCodeFilesClick(object sender, EventArgs e)
        {
            Settings.Default.FileTypeCob = showFileTypes_cob.Checked;
            Settings.Default.FileTypeTxt = showFileTypes_txt.Checked;
            Settings.Default.FileTypeSrc = showFileTypes_src.Checked;
            Settings.Default.FileTypeCustom = showFileTypes_custom.Text;
            Settings.Default.Save();

            if (UsedFileTypesChanged != null) UsedFileTypesChanged(this, new UsedFileTypesChangedEventArgs());

            RefreshFileView();
        }

        #endregion

        private void NavigateBackwardClick(object sender, EventArgs e)
        {
            codeBox.NavigateBackward();
            navigateForwardButton.Enabled = true;
        }

        private void NavigateForwardButtonClick(object sender, EventArgs e)
        {
            var success = codeBox.NavigateForward();
            if (!success)
                navigateForwardButton.Enabled = false;
        }

        private void FindNextButtonClick(object sender, EventArgs e)
        {
            TrySearch(false);
        }

        private void FindPreviousButtonClick(object sender, EventArgs e)
        {
            TrySearch(false, true);
        }
    }
}
