using System;
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;
using Model.File;

namespace Canal.UserControls.WordInfoViews
{
    public partial class VariableTreeView : UserControl
    {
        public event EventHandler<Variable> OnVariableSelected;

        public event EventHandler<Variable> OnVariableDoubleClicked;

        private const int LevelWidth = 20;

        private const int NameWidth = 280;

        private const int PicWidth = 160;

        private const int RedefinesWidth = 100;

        private bool _doubleClicked;

        public VariableTreeView()
        {
            InitializeComponent();
            SetUpTreeView();
        }

        public void SetTreeWithSelection(TreeNode node, TreeNode selectedTreeNode)
        {
            VariableInfoTreeView.Nodes.Add(node);
            VariableInfoTreeView.ExpandAll();
            VariableInfoTreeView.SelectedNode = selectedTreeNode;
        }


        public void ScrollToSelectedNode()
        {
            VariableInfoTreeView.SelectedNode.EnsureVisible();
        }

        public void SetTree(List<TreeNode> nodes)
        {
            VariableInfoTreeView.Nodes.AddRange(nodes.ToArray());
            VariableInfoTreeView.ExpandAll();
        }

        private void SetUpTreeView()
        {
            VariableInfoTreeView.DrawMode = TreeViewDrawMode.OwnerDrawText;
            VariableInfoTreeView.DrawNode += VariableInfoTreeViewOnDrawNode;

            VariableInfoTreeView.NodeMouseDoubleClick += (sender, args) =>
            {
                if (VariableInfoTreeView.SelectedNode == null) return;
                var clickedVariable = VariableInfoTreeView.SelectedNode.Tag as Variable;
                if (clickedVariable != null && OnVariableDoubleClicked != null)
                    OnVariableDoubleClicked(this, clickedVariable);
            };

            VariableInfoTreeView.AfterSelect += (sender, args) =>
            {
                if (VariableInfoTreeView.SelectedNode == null) return;
                var clickedVariable = VariableInfoTreeView.SelectedNode.Tag as Variable;
                if (clickedVariable != null && OnVariableSelected != null)
                    OnVariableSelected(this, clickedVariable);
            };

            // Keep tree from collapsing & expanding on double click
            VariableInfoTreeView.MouseDown += (sender, args) => _doubleClicked = args.Button == MouseButtons.Left && args.Clicks == 2;
            VariableInfoTreeView.BeforeExpand += (sender, args) => args.Cancel = args.Action == TreeViewAction.Expand ? _doubleClicked : args.Cancel;
            VariableInfoTreeView.BeforeCollapse += (sender, args) => args.Cancel = args.Action == TreeViewAction.Collapse ? _doubleClicked : args.Cancel;

            VariableInfoTreeView.HideSelection = false;

        }

        private void VariableInfoTreeViewOnDrawNode(object sender, DrawTreeNodeEventArgs e)
        {
            var variable = e.Node.Tag as Variable;
            if (variable == null)
                return;

            var nameWidth = NameWidth - e.Node.Level * LevelWidth;

            var font = e.Node.NodeFont ?? VariableInfoTreeView.Font;

            DrawBackground(e);
            DrawLevel(e, variable, font);
            DrawName(e, variable, font, nameWidth);
            DrawRedefines(e, variable, font, nameWidth);
            DrawPic(e, variable, font, nameWidth);
        }

        private void DrawBackground(DrawTreeNodeEventArgs e)
        {
            var selectedBack = Color.FromArgb(255, 255, 188, 112);

            e.Graphics.FillRectangle(
                e.Node.IsSelected ? new SolidBrush(selectedBack) : new SolidBrush(VariableInfoTreeView.BackColor),
                e.Bounds);
        }

        private static void DrawLevel(DrawTreeNodeEventArgs e, Variable variable, Font font)
        {
            var selectedForeLight = Color.FromArgb(255, 229, 138, 35);

            TextRenderer.DrawText(e.Graphics,
                variable.VariableLevel.ToString("D2"),
                font,
                new Rectangle(e.Bounds.X, e.Bounds.Y, LevelWidth, e.Bounds.Height),
                (e.State & TreeNodeStates.Selected) != 0 ? selectedForeLight : Color.DarkGray,
                Color.Empty,
                TextFormatFlags.VerticalCenter);
        }

        private static void DrawName(DrawTreeNodeEventArgs e, Variable variable, Font font, int nameWidth)
        {
            var selectedFore = Color.FromArgb(255, 207, 100, 1);

            TextRenderer.DrawText(e.Graphics,
                variable.VariableName + (variable.Occurs > 1 ? "  [" + variable.Occurs + "]" : ""),
                font,
                new Rectangle(e.Bounds.X + LevelWidth, e.Bounds.Y, nameWidth, e.Bounds.Height),
                (e.State & TreeNodeStates.Selected) != 0 ? selectedFore : e.Node.ForeColor,
                Color.Empty,
                TextFormatFlags.VerticalCenter);
        }

        private static void DrawRedefines(DrawTreeNodeEventArgs e, Variable variable, Font font, int nameWidth)
        {
            if (variable.Redefines != null)
                TextRenderer.DrawText(e.Graphics,
                    "↺ " + variable.Redefines.VariableName,
                    font,
                    new Rectangle(e.Bounds.X + LevelWidth + nameWidth - RedefinesWidth, e.Bounds.Y, RedefinesWidth + PicWidth,
                        e.Bounds.Height),
                    Color.DarkBlue,
                    Color.Empty,
                    TextFormatFlags.VerticalCenter | TextFormatFlags.EndEllipsis);
        }

        private static void DrawPic(DrawTreeNodeEventArgs e, Variable variable, Font font, int nameWidth)
        {
            if (variable.Picture != null)
                TextRenderer.DrawText(e.Graphics,
                    variable.Picture.ToString(),
                    font,
                    new Rectangle(e.Bounds.X + LevelWidth + nameWidth, e.Bounds.Y, PicWidth, e.Bounds.Height),
                    Color.DarkGray,
                    Color.Empty,
                    TextFormatFlags.VerticalCenter | TextFormatFlags.EndEllipsis);
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
