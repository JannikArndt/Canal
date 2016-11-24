namespace Canal.UserControls.WordInfoViews
{
    using System.Windows.Forms;
    using Model.File;
    using Util;

    public partial class VariableInfo : UserControl
    {
        private readonly FileControl _parent;
        private TreeNode _selectedNode;

        public VariableInfo(Variable variable, FileControl parent)
        {
            InitializeComponent();
            _parent = parent;
            var treeNode = FillVariableTreeView(variable);

            VariableInfoTreeView.SetTreeWithSelection(treeNode, _selectedNode);

            VariableInfoTreeView.OnVariableDoubleClicked += (sender, clickedVariable) =>
            {
                // TODO find better way to open VariableUsageWindow
                if (ModifierKeys == Keys.Shift)
                {
                    // shift + double click on variable name => open reference view
                    _parent.MainWindow.OpenVariableUsageWindow(clickedVariable);
                }
                else
                {
                    // simple double click on variable name => open file of definition
                    var filePath = (clickedVariable.Root ?? clickedVariable).CopyReference.FilePath;
                    _parent.MainWindow.OpenFile(filePath, clickedVariable);
                }
            };
        }

        public void ScrollToSelectedVariable()
        {
            VariableInfoTreeView.ScrollToSelectedNode();
        }

        private TreeNode FillVariableTreeView(Variable variable)
        {
            TreeNode newNode = VariablesUtil.Instance.ConvertToTreeNode(variable);

            // Saving the selected node is necessary to set it as the selected node in the VariableInfoTree later.
            // However, using a field is a little bit dirty as this method is already returning something. But the
            // only completely right way would be creating a new type like TreeNodeWithSelection which seems pretty
            // overdone so I sticked to this way.
            _selectedNode = newNode;

            if (variable.ParentVariable != null)
            {
                // save variable node
                var temp = newNode;
                var parent = variable.ParentVariable;

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

            return newNode;
        }

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            VariableInfoTreeView.Dispose();

            if (disposing && (components != null))
            {
                components.Dispose();
            }

            base.Dispose(disposing);
        }
    }
}
