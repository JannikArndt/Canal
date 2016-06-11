using Canal.Properties;
using FastColoredTextBoxNS.Events;
using Model;
using System;
using System.Drawing;
using System.Windows.Forms;
using Util;

namespace Canal.UserControls
{
    public partial class TableOfContents : UserControl
    {
        private readonly TableOfContentsWorker _tableOfContentsWorker;
        private SortKind _tocSort = SortKind.Alphabetical;
        private readonly CobolFile _cobolFile;
        private bool _userIsStillWaitingForPerformsTreeView;
        private PictureBox loader;

        public event EventHandler<WordSelectedEventArgs> OnWordSelected;

        public TableOfContents(CobolFile cobolFile)
        {
            InitializeComponent();
            _cobolFile = cobolFile;

            loader = new PictureBox
            {
                Image = Resources.loader,
                SizeMode = PictureBoxSizeMode.CenterImage,
                Dock = DockStyle.Fill,
                Size = new Size(364, 289)
            };

            _tableOfContentsWorker = new TableOfContentsWorker(_cobolFile);
            SortToc(SortKind.Alphabetical);
        }

        private void SortToc(SortKind kind)
        {
            _tocSort = kind;
            _userIsStillWaitingForPerformsTreeView = false;


            if (_cobolFile.CobolTree == null)
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
                tocTreeView.Controls.Add(loader);

                _userIsStillWaitingForPerformsTreeView = true;

                // wait for event
                _tableOfContentsWorker.PerformTreeIsBuilt += (sender, args) =>
                {
                    // if user has switched to another view, do not disturb him
                    if (!_userIsStillWaitingForPerformsTreeView) return;

                    var performTreeRootNode = (TreeNode)args.Result;
                    performTreeRootNode.ExpandAll();
                    tocTreeView.Nodes.Add(performTreeRootNode);
                    tocTreeView.Controls.Remove(loader);
                };
            }
            else
            {
                tocTreeView.Controls.Remove(loader);
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

        private void TocSearchTextBox_TextChanged(object sender, EventArgs e)
        {
            SortToc(_tocSort);
        }

        private void tocTreeView_AfterSelect(object sender, TreeViewEventArgs e)
        {
            if (OnWordSelected != null)
                OnWordSelected(this, new WordSelectedEventArgs(tocTreeView.SelectedNode.Text));
        }
    }
}
