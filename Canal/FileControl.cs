using Canal.Events;
using Canal.Properties;
using Canal.UserControls;
using FastColoredTextBoxNS.Events;
using System;
using System.Windows.Forms;

namespace Canal
{
    using CobolTree;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text.RegularExpressions;
    using Utils;

    public partial class FileControl : UserControl
    {
        public CobolFile CobolFile { get; private set; }

        public event EventHandler<UsedFileTypesChangedEventArgs> UsedFileTypesChanged;

        private readonly MainWindow _parent;

        public FileControl(CobolFile file, MainWindow parent)
        {
            InitializeComponent();
            try
            {
                _parent = parent;
                CobolFile = file;
                codeBox.SetFile(file);
                codeBox.KeyDown += searchBox_KeyDown;
                codeBox.WordSelected += CodeBoxOnWordSelected;
                searchBox.Text = Resources.SearchPlaceholder;

                treeView.Nodes.Add(CobolFile.CobolTree.AsTreeNodes);
                treeView.ExpandAll();

                performsTreeView.Nodes.Add(ReferenceUtil.GetPerformTree(file));
                performsTreeView.ExpandAll();

                // TODO insert friendly advise if copys are unresolved
                ShowProceduresTreeView();

                ShowVariablesTreeView();

                infoDataGridView.DataSource = CobolFile.Infos.ToArray();

                filesTreeView.Nodes.AddRange(FileUtil.GetDirectoryStructure());
                filesTreeView.ExpandAll();
                filesTabSearchBox.Text = Resources.SearchPlaceholder;
            }
            catch (Exception exception)
            {
                ErrorHandling.Exception(exception);
                MessageBox.Show(Resources.ErrorMessage_FileControl_Constructor + exception.Message, Resources.Error, MessageBoxButtons.OK);
            }
        }

        private void CodeBoxOnWordSelected(object sender, WordSelectedEventArgs eventArgs)
        {
            infoTabPage.Controls.Clear();
            infoTabPage.Controls.Add(new WordInfo(eventArgs.Word, this) { Dock = DockStyle.Fill });
        }

        public CodeBox CodeBox
        {
            get { return codeBox; }
        }

        #region Search Box

        private void seachBox_TextChanged(object sender, EventArgs e)
        {
            if ((bool)searchBox.Tag)
                codeBox.FindNext(searchBox.Text, false, searchWithRegEx.Checked, false, true);
        }

        private void searchBox_KeyDown(object sender, KeyEventArgs e)
        {
            switch (e.KeyCode)
            {
                case Keys.F3:
                    codeBox.FindNext(searchBox.Text, false, searchWithRegEx.Checked, false);
                    return;
                case Keys.Escape:
                    searchBox.Tag = false;
                    // codeBox.Selection = new Range(codeBox, Place.Empty, Place.Empty);
                    // codeBox.Invalidate();
                    searchBox.Text = string.Empty;
                    searchBox.Tag = true;
                    return;
            }
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
            var treeNode = treeView.SelectedNode as CobolTreeNode;
            if (treeNode == null)
                codeBox.DoRangeVisible(codeBox.GetRange(0, 0));
            else
                codeBox.FindNext(@"^.{7}" + treeNode.Text + @"(\.| +USING)", false, true, false, true);
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

        #endregion

        #region Button Clicks

        private void ResolveCopysButton_Click(object sender, EventArgs e)
        {
            Cursor = Cursors.WaitCursor;
            ReferenceUtil.ResolveCopys(CobolFile);
            codeBox.Text = CobolFile.Text;

            ShowVariablesTreeView();

            ShowProceduresTreeView();

            ResolveCopysButton.Enabled = false;

            Cursor = Cursors.Default;
        }

        private void ExportTocClick(object sender, EventArgs e)
        {
            Clipboard.SetText(treeView.ToText());
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

        private void ShowProceduresTreeView()
        {
            proceduresTreeView.Nodes.Clear();

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

                    foreach (var key in varDict.Keys.OrderBy(r => r.Name))
                    {
                        var rootVarNode = new TreeNode(key.Name);
                        foreach (var variable in varDict[key])
                            rootVarNode.Nodes.Add(new TreeNode(variable.Level.ToString("D2") + "  " + variable.Name + " " + procedure.Variables[variable].ToShortString()));

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
            treeView.ExpandAll();
        }

        private void TocCollapseAllButton_Click(object sender, EventArgs e)
        {
            treeView.CollapseAll();
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

        private void filesTabSearchBox_TextChanged(object sender, EventArgs e)
        {
            if (((ToolStripTextBox)sender).Text == Resources.SearchPlaceholder)
                return;
            filesTreeView.Nodes.Clear();
            filesTreeView.Nodes.AddRange(FileUtil.GetDirectoryStructure(((ToolStripTextBox)sender).Text));
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

            _parent.OpenFile(((FileReference)filesTreeView.SelectedNode.Tag).FullPath);
        }

        private void filesTreeView_KeyUp(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter)
                _parent.OpenFile(((FileReference)filesTreeView.SelectedNode.Tag).FullPath);
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
            var nodes = FileUtil.GetDirectoryStructure(searchText);
            filesTreeView.Nodes.Clear();
            filesTreeView.Nodes.AddRange(nodes);
            filesTreeView.ExpandAll();
        }
    }
}
