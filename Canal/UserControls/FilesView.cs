using Canal.Properties;
using Model.References;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;
using Util;

namespace Canal.UserControls
{
    public partial class FilesView : UserControl
    {
        public FilesView()
        {
            InitializeComponent();

            InitializeFilesView();
        }

        private void InitializeFilesView()
        {
            LoadUsedFileTypesFromSettings();

            FileUtil.Instance.ReduceDirectoriesToAllowedFiles();

            RefreshFileView();

            filesTabSearchBox.Text = Resources.SearchPlaceholder;

            FileUtil.Instance.FileCacheChanged += (sender, args) =>
            {
                FileUtil.Instance.ReduceDirectoriesToAllowedFiles();

                if (filesTreeView.InvokeRequired)
                    filesTreeView.Invoke(new MethodInvoker(RefreshFileView));
                else
                    RefreshFileView();
            };
        }

        #region Searchbox Focus

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

        private void FilesTabSearchBoxTextChanged(object sender, EventArgs e)
        {
            if (((ToolStripTextBox)sender).Text == Resources.SearchPlaceholder || !(bool)((ToolStripTextBox)sender).Tag)
                return;

            RefreshFileView();

            SelectSingleResult();
        }

        private void SelectSingleResult()
        {
            if (filesTreeView.Nodes.Count != 1 || filesTreeView.Nodes[0].Nodes.Count != 1) return;

            filesTreeView.SelectedNode = filesTreeView.Nodes[0].Nodes[0];
            filesTreeView.Focus();
        }

        #region Open File

        private void FilesTreeViewDoubleClick(object sender, EventArgs e)
        {
            if (filesTreeView.SelectedNode == null || filesTreeView.SelectedNode.Tag == null)
                return;

            TryOpenFile(((FileReference)filesTreeView.SelectedNode.Tag).FilePath);
        }

        private void FilesTreeViewKeyUp(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter && filesTreeView.SelectedNode != null && filesTreeView.SelectedNode.Tag != null)
            {
                TryOpenFile(((FileReference)filesTreeView.SelectedNode.Tag).FilePath);
            }
        }

        private void TryOpenFile(string filepath)
        {
            var parent = Parent.Parent.Parent as MainWindow;
            if (parent != null)
                parent.OpenFile(filepath);
        }

        #endregion

        #region Used File Types

        private void FileTypeClicked(object sender, EventArgs e)
        {
            SaveUsedFileTypesToSettings();
            FileUtil.Instance.ReduceDirectoriesToAllowedFiles();
            RefreshFileView();
        }

        private void SaveUsedFileTypesToSettings()
        {
            Util.Properties.Settings.Default.FileTypeCob = showFileTypes_cob.Checked;
            Util.Properties.Settings.Default.FileTypeCbl = showFileTypes_cbl.Checked;
            Util.Properties.Settings.Default.FileTypeTxt = showFileTypes_txt.Checked;
            Util.Properties.Settings.Default.FileTypeSrc = showFileTypes_src.Checked;
            Util.Properties.Settings.Default.FileTypeCustom = showFileTypes_custom.Text;
            Util.Properties.Settings.Default.ShowFileType = showFileTypes.Checked;
            Util.Properties.Settings.Default.Save();
        }

        /// <summary>
        /// Updates the DropDownButton in the Files-tab with the settings stored in Settings.Default.FileType* and refreshes the Files-tab
        /// </summary>
        private void LoadUsedFileTypesFromSettings()
        {
            showFileTypes_cob.Checked = Util.Properties.Settings.Default.FileTypeCob;
            showFileTypes_cbl.Checked = Util.Properties.Settings.Default.FileTypeCbl;
            showFileTypes_txt.Checked = Util.Properties.Settings.Default.FileTypeTxt;
            showFileTypes_src.Checked = Util.Properties.Settings.Default.FileTypeSrc;
            showFileTypes_custom.Text = Util.Properties.Settings.Default.FileTypeCustom;
            showFileTypes.Checked = Util.Properties.Settings.Default.ShowFileType;
        }

        #endregion

        /// <summary>
        /// Refreshes the Files-tab
        /// </summary>
        private void RefreshFileView()
        {

            var expanded = filesTreeView.Nodes.Count > 0 && filesTreeView.Nodes[0].IsExpanded;

            var searchText = filesTabSearchBox == null || filesTabSearchBox.Text == Resources.SearchPlaceholder ? "" : filesTabSearchBox.Text;
            var nodes = FileUtil.Instance.GetDirectoryStructure(searchText);

            if (NodesEqual(nodes, filesTreeView.Nodes))
                return;

            filesTreeView.Nodes.Clear();
            filesTreeView.Nodes.AddRange(nodes);

            if (expanded || searchText != "")
                filesTreeView.ExpandAll();
        }

        private bool NodesEqual(IReadOnlyCollection<TreeNode> nodes, TreeNodeCollection treeNodeCollection)
        {
            // different count of nodes => not equal
            if (nodes.Count != treeNodeCollection.Count)
                return false;

            // different texts for any node? (only top level) => not equal
            if (!nodes.Where((node1, nodeIndex) => node1.Text != treeNodeCollection[nodeIndex].Text).Any())
                return false;

            // continue for next level
            return treeNodeCollection.Cast<TreeNode>().Any(child => !NodesEqual(nodes, child.Nodes));
        }

        private void CollapseAllToolStripButton_Click(object sender, EventArgs e)
        {
            filesTreeView.CollapseAll();
        }

        private void ExpandAllToolStripButton_Click(object sender, EventArgs e)
        {
            filesTreeView.ExpandAll();
        }
    }
}
