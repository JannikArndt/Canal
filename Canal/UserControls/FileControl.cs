﻿using Canal.Properties;
using FastColoredTextBoxNS;
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
using Util;
using Util.Events;

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

                AnalyzeFile();

                // initialize FastColoredTextBox
                codeBox.Font = SourceCodePro.Instance.Regular();
                codeBox.SetTextAsync(CobolFile.Text);

                codeBox.KeyDown += HandleKeyDown;
                codeBox.WordSelected += CodeBoxOnWordSelected;
                codeBox.UndoRedoStateChanged += CodeBoxOnUndoRedoStateChanged;
                codeBox.VisualMarkerClick += CodeBoxOnPerformMarkerClick;
                codeBox.TextChanged += (sender, args) =>
                {
                    if (!UnsavedChanges && SavedVersionChanged != null)
                        SavedVersionChanged(sender, args);
                    UnsavedChanges = true;
                    saveButton.Enabled = true;
                };

                searchBox.Text = Resources.SearchPlaceholder;
            }
            catch (Exception exception)
            {
                Logger.Error("Error initializing file control for CobolFile {0}: {1}", filename, exception.Message);
                MessageBox.Show(Resources.ErrorMessage_FileControl_Constructor + exception.Message, Resources.Error, MessageBoxButtons.OK);
            }
        }

        public void AnalyzeFile()
        {
            if (CobolFile.FileReference == null) return;

            var worker = new BackgroundWorker();
            worker.DoWork += new Analyzer().AnalyzeFile;
            worker.RunWorkerCompleted += InitTabs;
            worker.RunWorkerCompleted += SetFoldingMarkers;
            worker.RunWorkerAsync(CobolFile);
        }

        private void SetFoldingMarkers(object sender, RunWorkerCompletedEventArgs runWorkerCompletedEventArgs)
        {
            try
            {
                var foldingAreas = new List<CobolTreeNode>(CobolFile.CobolTree.GetAllDivisions())
                                                   .Concat(CobolFile.CobolTree.GetAllSections())
                                                   .Concat(CobolFile.CobolTree.GetAllProcedures());

                foreach (var area in foldingAreas)
                {
                    codeBox.SetFoldingMarker(area.StartIndex, area.EndIndex, area.Name);
                }
            }
            catch (Exception exception)
            {
                Logger.Error("Error setting folding markers: {0}.", exception.Message);
            }
        }

        private void CodeBoxOnPerformMarkerClick(object sender, VisualMarkerEventArgs visualMarkerEventArgs)
        {
            var clickedLineText = visualMarkerEventArgs.Marker.Text;

            var performMatch = Regex.Match(clickedLineText, Constants.Perform, RegexOptions.IgnoreCase);
            if (performMatch.Success)
            {
                codeBox.FindNext(@"^.{7}" + performMatch.Groups[1].Value + @"(\.| +USING)", false, true, false, true);
                return;
            }

            var gotoMatch = Regex.Match(clickedLineText, Constants.GoTo, RegexOptions.IgnoreCase);
            if (gotoMatch.Success)
            {
                codeBox.FindNext(@"^.{7}" + gotoMatch.Groups[1].Value + @"(\.| +USING)", false, true, false, true);
                return;
            }

            var callmatch = Regex.Match(clickedLineText, Constants.Call, RegexOptions.IgnoreCase);
            if (callmatch.Success)
            {
                var fileRef = FileUtil.Instance.GetFileReferences(callmatch.Groups[4].Value);
                if (fileRef.Count == 1)
                    MainWindow.OpenFile(fileRef.First().FilePath);
                else if (fileRef.Count > 0)
                    MessageBox.Show(
                        "There are multiple files matching this program name. Try files tab to open program.",
                        "Multiple Matching Files Found", MessageBoxButtons.OK);
                else
                    MessageBox.Show("File could not be found.", "File Not Found", MessageBoxButtons.OK);
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

        public override string ToString()
        {
            return string.Format("FileControl: {0}", CobolFile.Name);
        }

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
            CobolFile.Text = codeBox.Text;
            UnsavedChanges = false;
            saveButton.Enabled = false;
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
            showFileTypes_cob.Checked = Util.Properties.Settings.Default.FileTypeCob;
            showFileTypes_txt.Checked = Util.Properties.Settings.Default.FileTypeTxt;
            showFileTypes_src.Checked = Util.Properties.Settings.Default.FileTypeSrc;
            showFileTypes_custom.Text = Util.Properties.Settings.Default.FileTypeCustom;

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

        private void CodeBoxOnUndoRedoStateChanged(object sender, EventArgs eventArgs)
        {
            undoButton.Enabled = codeBox.UndoEnabled;
            redoButton.Enabled = codeBox.RedoEnabled;
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
            var treeNode = variablesTreeView.SelectedNode;
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

        private void ExportTocClick(object sender, EventArgs e)
        {
            Clipboard.SetText(tocTreeView.ToText());
        }

        private void CopyProceduresClick(object sender, EventArgs e)
        {
            Clipboard.SetText(proceduresTreeView.ToText());
        }

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

        private void undoButton_Click(object sender, EventArgs e)
        {
            codeBox.Undo();
        }

        private void redoButton_Click(object sender, EventArgs e)
        {
            codeBox.Redo();
        }

        private void FindNextButtonClick(object sender, EventArgs e)
        {
            TrySearch(false);
        }

        private void FindPreviousButtonClick(object sender, EventArgs e)
        {
            TrySearch(false, true);
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

            foreach (var variable in CobolFile.GetLocalRootVariables())
            {
                workingStorageSectionTreeNode.Nodes.Add(VariablesUtil.Instance.ConvertToTreeNode(variable));
            }

            var linkageSectionTreeNode = new TreeNode("From Records");

            foreach (var variable in CobolFile.GetCopiedRootVariables())
            {
                linkageSectionTreeNode.Nodes.Add(VariablesUtil.Instance.ConvertToTreeNode(variable));
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

            tocTreeView.Nodes.Add(CobolTreeBuilder.ConvertToTreeNodes(CobolFile.CobolTree, CobolFile.Name));
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
            Util.Properties.Settings.Default.FileTypeCob = showFileTypes_cob.Checked;
            Util.Properties.Settings.Default.FileTypeTxt = showFileTypes_txt.Checked;
            Util.Properties.Settings.Default.FileTypeSrc = showFileTypes_src.Checked;
            Util.Properties.Settings.Default.FileTypeCustom = showFileTypes_custom.Text;
            Util.Properties.Settings.Default.Save();

            if (UsedFileTypesChanged != null) UsedFileTypesChanged(this, new UsedFileTypesChangedEventArgs());

            RefreshFileView();
        }

        #endregion

        #region Functions

        /// <summary>
        /// Displays a filter window and then insert the selected copybook-references into the text and refreshes the view
        /// </summary>
        public void InsertCopybooksIntoSource()
        {
            var worker = new BackgroundWorker();

            try
            {
                var references = TextUtil.Instance.FindCopyReferences(CobolFile.Text).ToList();

                var filter = new FileReferenceFilter(references)
                {
                    Text = Resources.FilterCopybooksWindowTitle
                };
                filter.ShowDialog();

                if (filter.Result != null && filter.Result.Any())
                {
                    worker.DoWork += (sender, args) =>
                    {
                        foreach (var copyRef in filter.Result)
                        {
                            TextUtil.Instance.Insert(CobolFile, copyRef);
                        }
                    };

                    worker.RunWorkerCompleted += (sender, args) => codeBox.SetTextAsync(CobolFile.Text);

                    worker.RunWorkerAsync();
                }
            }
            catch (Exception exception)
            {
                Logger.Error("Error trying to insert copybooks into source: {0}.", exception.Message);
                MessageBox.Show(string.Format("Error trying to insert copybooks into source: {0}.", exception.Message),
                    Resources.Error, MessageBoxButtons.OK);
            }
        }

        #endregion

        private void newButton_Click(object sender, EventArgs e)
        {
            MainWindow.New();
        }

        private void openButton_Click(object sender, EventArgs e)
        {
            MainWindow.OpenFile();
        }

        private void saveButton_Click(object sender, EventArgs e)
        {
            MainWindow.Save();
        }

        private void formatCodeButton_Click(object sender, EventArgs e)
        {
            var selection = new Range(codeBox, codeBox.Selection.Start, codeBox.Selection.End);
            if (codeBox.Selection.IsEmpty)
                codeBox.SelectAll();
            codeBox.DoAutoIndent();
            codeBox.Selection = selection;
        }

        private void TocSortAlphabeticallyButton_Click(object sender, EventArgs e)
        {
            if (CobolFile.CobolTree == null)
                return;

            var query = tocSearchTextBox.Text != Resources.SearchPlaceholder ? tocSearchTextBox.Text : "";

            TocSortAlphabeticallyButton.Checked = true;
            TocSortHierarchicallyButton.Checked = false;
            tocTreeView.Nodes.Clear();
            tocTreeView.Nodes.Add(CobolTreeBuilder.ConvertToFlatToc(CobolFile.CobolTree, CobolFile.Name, query));
            // tocTreeView.Sort();
            tocTreeView.ExpandAll();
        }

        private void TocSortHierarchicallyButton_Click(object sender, EventArgs e)
        {
            if (CobolFile.CobolTree == null)
                return;

            var query = tocSearchTextBox.Text != Resources.SearchPlaceholder ? tocSearchTextBox.Text : "";

            TocSortAlphabeticallyButton.Checked = false;
            TocSortHierarchicallyButton.Checked = true;

            tocTreeView.Nodes.Clear();
            tocTreeView.Nodes.Add(CobolTreeBuilder.ConvertToTreeNodes(CobolFile.CobolTree, CobolFile.Name, query));
            tocTreeView.ExpandAll();
        }

        private void tocSearchTextBox_TextChanged(object sender, EventArgs e)
        {
            var query = ((ToolStripTextBox)sender).Text;

            if (query == Resources.SearchPlaceholder || CobolFile.CobolTree == null)
                return;

            tocTreeView.Nodes.Clear();
            tocTreeView.Nodes.Add(
                TocSortAlphabeticallyButton.Checked
                ? CobolTreeBuilder.ConvertToFlatToc(CobolFile.CobolTree, CobolFile.Name, query)
                : CobolTreeBuilder.ConvertToTreeNodes(CobolFile.CobolTree, CobolFile.Name, query));
            tocTreeView.ExpandAll();
        }
    }
}
