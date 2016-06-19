using Model;
using System.Drawing;
using System.Windows.Forms;
using Model.File;
using Util;

namespace Canal.UserControls.WordInfoViews
{
    public partial class VariableInfo : UserControl
    {
        private readonly FileControl _parent;

        public VariableInfo(Variable variable, FileControl parent)
        {
            InitializeComponent();
            _parent = parent;
            FillVariableTreeView(variable);
        }

        private void FillVariableTreeView(Variable variable)
        {
            var newNode = VariablesUtil.Instance.ConvertToTreeNode(variable);

            var parent = variable;

            if (variable.ParentVariable != null)
            {
                // save variable node
                var temp = newNode;
                parent = variable.ParentVariable;

                // new node for parent variable
                newNode = new TreeNode(variable.ParentVariable.GetLevelAndName()) { Tag = variable.ParentVariable };

                // add parents' children (siblings and self)
                foreach (var sibling in variable.ParentVariable.Variables)
                {
                    newNode.Nodes.Add(sibling == variable
                        ? temp
                        : new TreeNode(sibling.GetLevelAndName()) { Tag = sibling });
                }

                // go further up, add grandparents etc.
                while (parent.ParentVariable != null)
                {
                    newNode = new TreeNode(parent.ParentVariable.GetLevelAndName(), new[] { newNode })
                    {
                        Tag = parent.ParentVariable
                    };

                    parent = parent.ParentVariable;
                }
            }

            VariableInfoTreeView.DrawMode = TreeViewDrawMode.OwnerDrawText;
            VariableInfoTreeView.DrawNode += VariableInfoTreeViewOnDrawNode;
            VariableInfoTreeView.Nodes.Add(newNode);
            VariableInfoTreeView.ExpandAll();

            if (parent.CopyReference != null)
            {
                gotoFileButton.Visible = true;
                gotoFileButton.Click += (sender, args) => _parent.MainWindow.OpenFile(parent.CopyReference.FilePath, variable);
            }

            VariableInfoTreeView.NodeMouseDoubleClick += (sender, args) =>
            {
                var vari = VariableInfoTreeView.SelectedNode.Tag as Variable;
                if (vari != null)
                    _parent.FindInCodeBox(vari.VariableName, false, false, false, true);
            };
        }

        private void VariableInfoTreeViewOnDrawNode(object sender, DrawTreeNodeEventArgs e)
        {
            var variable = e.Node.Tag as Variable;
            if (variable == null)
                return;

            var levelWidth = 20;
            var nameWidth = 280 - e.Node.Level * 20;
            var picWidth = 160;

            // Level
            TextRenderer.DrawText(e.Graphics,
                                  variable.VariableLevel.ToString("D2"),
                                  e.Node.NodeFont,
                                  new Rectangle(e.Bounds.X, e.Bounds.Y, levelWidth, e.Bounds.Height),
                                  Color.DarkGray,
                                  Color.Empty,
                                  TextFormatFlags.VerticalCenter);

            // Name
            TextRenderer.DrawText(e.Graphics,
                                  variable.VariableName,
                                  e.Node.NodeFont,
                                  new Rectangle(e.Bounds.X + levelWidth, e.Bounds.Y, nameWidth, e.Bounds.Height),
                                  (e.State & TreeNodeStates.Selected) != 0 ? SystemColors.HighlightText : e.Node.ForeColor,
                                  Color.Empty,
                                  TextFormatFlags.VerticalCenter);

            // PIC
            if (variable.Picture != null)
                TextRenderer.DrawText(e.Graphics,
                                      variable.Picture.ToString(),
                                      e.Node.NodeFont,
                                      new Rectangle(e.Bounds.X + levelWidth + nameWidth, e.Bounds.Y, picWidth, e.Bounds.Height),
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
