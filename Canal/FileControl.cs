using Canal.Properties;
using FastColoredTextBoxNS;
using System;
using System.Windows.Forms;

namespace Canal
{
    public partial class FileControl : UserControl
    {
        public CobolFile CobolFile { get; set; }

        public FileControl(CobolFile file)
        {
            InitializeComponent();
            CobolFile = file;
            codeBox.SetFile(file);
            searchBox.Text = Resources.SearchPlaceholder;

            treeView.Nodes.Add(CobolFile.CobolTree.AsTreeNodes);
            treeView.ExpandAll();
        }

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
    }
}
