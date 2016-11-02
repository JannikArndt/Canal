using Canal.Properties;
using FastColoredTextBoxNS.Events;
using Model.File;
using System;
using System.Collections.Generic;
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

        /// <summary>
        /// Compares the given possible node names withe the ToC tree and highlights the first match.
        /// </summary>
        /// <param name="nodeTexts">A list of possible node names.</param>
        public void HighlightFirstMatchingNode(List<string> nodeTexts)
        {
            var node = nodeTexts.Select(FindNodeByName).FirstOrDefault(n => n != null);

            //No need to proceed if node is already highlighted or not found
            if (node == null || node == _lastHighlightedNode) return;

            node.BackColor = Color.FromArgb(1, 155, 205, 155); //equals the runtime folding indicator color of the fast colored textbox which can't be referenced here because it's generated at runtime
            node.ForeColor = Color.FromArgb(1, 89, 140, 89);

            //Tidy up last highlighted node if possible
            if (_lastHighlightedNode != null)
            {
                _lastHighlightedNode.BackColor = Color.Transparent;
                _lastHighlightedNode.ForeColor = Color.Black;
            }

            //Set new node as currently highlighted
            _lastHighlightedNode = node;

            node.EnsureVisible();

        }


        /// <summary>
        /// Returns the first node with the given name or null if no matching node is found.
        /// </summary>
        /// <param name="name">The name.</param>
        /// <returns>The matching node or null.</returns>
        private TreeNode FindNodeByName(string name)
        {
            return
                tocTreeView.Nodes.Find("", true)
                    .FirstOrDefault(node => node.Text.ToLowerInvariant() == name.ToLowerInvariant());
        }
        #endregion

        private void tocTreeView_AfterSelect(object sender, TreeViewEventArgs e)
        {
            if (OnWordSelected != null)
                OnWordSelected(this, new WordSelectedEventArgs(tocTreeView.SelectedNode.Text));
        }
    }
}
