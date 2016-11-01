using Canal.Properties;
using FastColoredTextBoxNS.Events;
using Model.File;
using System;
using System.Drawing;
using System.Linq;
using System.Windows.Forms;
using Util;

namespace Canal.UserControls
{
    public partial class TableOfContents : UserControl
    {
        private TableOfContentsWorker _tableOfContentsWorker;
        private SortKind _tocSort = SortKind.Alphabetical;
        private CobolFile _cobolFile;
        private bool _userIsStillWaitingForPerformsTreeView;
        private readonly PictureBox _loader;
        private TreeNode _lastHighlightedNode;

        public event EventHandler<WordSelectedEventArgs> OnWordSelected;

        public TableOfContents()
        {
            InitializeComponent();

            _loader = new PictureBox
            {
                Image = Resources.loader,
                SizeMode = PictureBoxSizeMode.CenterImage,
                Dock = DockStyle.Fill,
                Size = new Size(364, 289)
            };

            tocSearchTextBox.Tag = false;
            tocSearchTextBox.Text = Resources.SearchPlaceholder;
            tocSearchTextBox.Tag = true;

            tocTreeView.Controls.Add(_loader);
        }

        public void SetCobolFile(CobolFile cobolFile)
        {
            if (IsDisposed)
                return;

            _cobolFile = cobolFile;
            _tableOfContentsWorker = new TableOfContentsWorker(_cobolFile);
            SortToc(Util.Properties.Settings.Default.TocSort);

            if (tocTreeView.Controls.Contains(_loader))
                tocTreeView.Controls.Remove(_loader);
        }

        private void SortToc(SortKind kind)
        {
            _tocSort = kind;
            Util.Properties.Settings.Default.TocSort = kind;
            Util.Properties.Settings.Default.Save();

            _userIsStillWaitingForPerformsTreeView = false;


            if (_cobolFile == null || _cobolFile.CobolTree == null)
            {
                tocTreeView.Nodes.Add(new TreeNode("Error: CobolTree could not be built.") { ForeColor = Color.Red });
                return;
            }

            var query = tocSearchTextBox.Text != Resources.SearchPlaceholder ? tocSearchTextBox.Text : "";

            TocSortAlphabeticallyButton.Checked = _tocSort == SortKind.Alphabetical;
            TocSortHierarchicallyButton.Checked = _tocSort == SortKind.BySections;
            TocSortByPerformsButton.Checked = _tocSort == SortKind.ByPerforms;

            tocTreeView.Nodes.Clear();
            var rootNode = _tableOfContentsWorker.GetToc(_tocSort, query);

            // null => perform tree is not built yet => wait for event
            if (rootNode == null)
            {
                // add loading image
                tocTreeView.Controls.Add(_loader);

                _userIsStillWaitingForPerformsTreeView = true;

                // wait for event
                _tableOfContentsWorker.PerformTreeIsBuilt += (sender, args) =>
                {
                    // if user has switched to another view, do not disturb him
                    if (!_userIsStillWaitingForPerformsTreeView) return;

                    var performTreeRootNode = (TreeNode)args.Result;
                    performTreeRootNode.ExpandAll();
                    tocTreeView.Nodes.Add(performTreeRootNode);
                    tocTreeView.Controls.Remove(_loader);
                };
            }
            else
            {
                tocTreeView.Controls.Remove(_loader);
                rootNode.ExpandAll();
                tocTreeView.Nodes.Add(rootNode);
            }
        }

        private void TocExpandAllButtonClick(object sender, EventArgs e)
        {
            tocTreeView.ExpandAll();
        }

        private void TocCollapseAllButtonClick(object sender, EventArgs e)
        {
            tocTreeView.CollapseAll();
        }

        private void ExportTocClick(object sender, EventArgs e)
        {
            Clipboard.SetText(tocTreeView.ToText());
        }

        private void TocSortAlphabeticallyButton_Click(object sender, EventArgs e)
        {
            SortToc(SortKind.Alphabetical);
        }

        private void TocSortBySectionsButtonClick(object sender, EventArgs e)
        {
            SortToc(SortKind.BySections);
        }

        private void TocSortByPerformsButtonClick(object sender, EventArgs e)
        {
            SortToc(SortKind.ByPerforms);
        }


        #region Searchbox

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

        private void TocSearchTextBox_TextChanged(object sender, EventArgs e)
        {
            if ((bool)tocSearchTextBox.Tag)
                SortToc(_tocSort);
        }

        public void HighlightNode(string nodeText)
        {
            var node = FindNodeByName(nodeText);
            if (_lastHighlightedNode != node && node != null)
            {
                node.BackColor = Color.Brown;
                node.ForeColor = Color.Crimson;

                if (_lastHighlightedNode != null)
                {
                    _lastHighlightedNode.BackColor = Color.Transparent;
                    _lastHighlightedNode.ForeColor = Color.Black;
                }
                _lastHighlightedNode = node;
                node.EnsureVisible();
                //tocTreeView.SelectedNode = FindNodeByName(nodeText);
            }
            //RefreshTree();
        }

        private TreeNode FindNodeByName(string name)
        {
            //return tocTreeView.Nodes.Find(name, true).First();
            var list =
                (from n in tocTreeView.Nodes.Find("", true).Cast<TreeNode>() where n.Text.ToLower().Equals(name.ToLower()) select n).ToList();
            return list.Any() ? list.First() : null;
        }
        #endregion

        private void tocTreeView_AfterSelect(object sender, TreeViewEventArgs e)
        {
            if (OnWordSelected != null)
                OnWordSelected(this, new WordSelectedEventArgs(tocTreeView.SelectedNode.Text));
        }
    }
}
