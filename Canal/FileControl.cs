using System;
using System.Windows.Forms;
using Canal.Properties;
using FastColoredTextBoxNS;

namespace Canal
{
    using System.Collections.Generic;
    using System.Linq;
    using System.Text.RegularExpressions;

    using CobolTree;
    using Utils;

    public partial class FileControl : UserControl
    {
        public CobolFile CobolFile { get; private set; }

        public FileControl(CobolFile file)
        {
            InitializeComponent();
            CobolFile = file;
            codeBox.SetFile(file);
            searchBox.Text = Resources.SearchPlaceholder;

            treeView.Nodes.Add(CobolFile.CobolTree.AsTreeNodes);
            treeView.ExpandAll();

            performsTree.Nodes.Add(ReferenceUtil.GetPerformTree(file));
            performsTree.ExpandAll();

            // TODO insert friendly advise if copys are unresolved
            ShowProceduresTreeView();

            ShowVariablesTreeView();
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
                    codeBox.Selection = new Range(codeBox, Place.Empty, Place.Empty);
                    codeBox.Invalidate();
                    searchBox.Text = "";
                    searchBox.Tag = true;
                    return;
            }
        }

        private void searchBox_Enter(object sender, EventArgs e)
        {
            if (searchBox.Text == Resources.SearchPlaceholder)
            {
                searchBox.Tag = false;
                searchBox.Text = "";
                searchBox.Tag = true;
            }
        }

        private void searchBox_Leave(object sender, EventArgs e)
        {
            if (string.IsNullOrWhiteSpace(searchBox.Text))
            {
                searchBox.Tag = false;
                searchBox.Text = Resources.SearchPlaceholder;
                searchBox.Tag = true;
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
            var treeNode = performsTree.SelectedNode;
            if (treeNode == null)
                codeBox.DoRangeVisible(codeBox.GetRange(0, 0));
            else
                codeBox.FindNext(treeNode.Text, false, false, false, true);
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
    }
}
