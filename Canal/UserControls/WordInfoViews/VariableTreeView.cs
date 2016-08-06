using Model.File;
using System;
using System.Drawing;
using System.Windows.Forms;

namespace Canal.UserControls.WordInfoViews
{
    public partial class VariableTreeView : UserControl
    {
        public event EventHandler<Variable> OnVariableSelected;

        private const int LevelWidth = 20;

        private const int NameWidth = 280;

        private const int PicWidth = 160;

        bool _doubleClicked;

        public VariableTreeView()
        {
            InitializeComponent();
            SetUpTreeView();
        }

        public void SetTree(TreeNode node)
        {
            VariableInfoTreeView.Nodes.Add(node);
            VariableInfoTreeView.ExpandAll();
        }

        private void SetUpTreeView()
        {
            VariableInfoTreeView.DrawMode = TreeViewDrawMode.OwnerDrawText;
            VariableInfoTreeView.DrawNode += VariableInfoTreeViewOnDrawNode;
            VariableInfoTreeView.NodeMouseDoubleClick += (sender, args) =>
            {
                var clickedVariable = VariableInfoTreeView.SelectedNode.Tag as Variable;
                if (clickedVariable != null && OnVariableSelected != null)
                    OnVariableSelected(this, clickedVariable);
            };

            // Keep tree from collapsing & expanding on double click
            VariableInfoTreeView.MouseDown += (sender, args) => _doubleClicked = args.Button == MouseButtons.Left && args.Clicks == 2;
            VariableInfoTreeView.BeforeExpand += (sender, args) => args.Cancel = args.Action == TreeViewAction.Expand ? _doubleClicked : args.Cancel;
            VariableInfoTreeView.BeforeCollapse += (sender, args) => args.Cancel = args.Action == TreeViewAction.Collapse ? _doubleClicked : args.Cancel;
        }

        private void VariableInfoTreeViewOnDrawNode(object sender, DrawTreeNodeEventArgs e)
        {
            var variable = e.Node.Tag as Variable;
            if (variable == null)
                return;

            var nameWidth = NameWidth - e.Node.Level * LevelWidth;

            // Level
            TextRenderer.DrawText(e.Graphics,
                                  variable.VariableLevel.ToString("D2"),
                                  e.Node.NodeFont,
                                  new Rectangle(e.Bounds.X, e.Bounds.Y, LevelWidth, e.Bounds.Height),
                                  Color.DarkGray,
                                  Color.Empty,
                                  TextFormatFlags.VerticalCenter);

            // Name
            TextRenderer.DrawText(e.Graphics,
                                  variable.VariableName,
                                  e.Node.NodeFont,
                                  new Rectangle(e.Bounds.X + LevelWidth, e.Bounds.Y, nameWidth, e.Bounds.Height),
                                  (e.State & TreeNodeStates.Selected) != 0 ? SystemColors.HighlightText : e.Node.ForeColor,
                                  Color.Empty,
                                  TextFormatFlags.VerticalCenter);

            // PIC
            if (variable.Picture != null)
                TextRenderer.DrawText(e.Graphics,
                                      variable.Picture.ToString(),
                                      e.Node.NodeFont,
                                      new Rectangle(e.Bounds.X + LevelWidth + nameWidth, e.Bounds.Y, PicWidth, e.Bounds.Height),
                                      Color.DarkGray,
                                      Color.Empty,
                                      TextFormatFlags.VerticalCenter);
        }

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            VariableInfoTreeView.Nodes.Clear();
            VariableInfoTreeView.Dispose();

            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }
    }
}
