using System;
using System.Windows.Forms;
using Util;
using Model.File;

namespace VariableUsageAnalyzer
{
    public sealed partial class VariableSelectionControl : UserControl
    {
        private TreeNode _selectedNode;

        private Variable _currentlySelectedVariable;

        public delegate void VariableSelectionOrSearchConfigChangedEventHandler(object sender, Variable variable, bool includeDirectAndIndirectChildVariables, bool includeRedefines);
        public event VariableSelectionOrSearchConfigChangedEventHandler VariableSelectionOrSearchConfigChanged;

        public VariableSelectionControl(Variable variable)
        {
            InitializeComponent();
            var treeNode = FillVariableTreeView(variable);
            Dock = DockStyle.Fill;
            VariableSelectionTreeView.Dock = DockStyle.Fill;
            VariableSelectionTreeView.SetTreeWithSelection(treeNode, _selectedNode);
            VariableSelectionTreeView.OnVariableSelected +=
                (sender, variable1) => { _currentlySelectedVariable = variable1; HandleVariableSelectionOrSearchConfigChanged();};
            includeDirectAndIndirectChildVariablesCheckBox.CheckedChanged +=
                (sender, args) => { HandleVariableSelectionOrSearchConfigChanged(); };
            includeRedefinesCheckBox.CheckedChanged +=
                (sender, args) => { HandleVariableSelectionOrSearchConfigChanged(); };

        }

        private void HandleVariableSelectionOrSearchConfigChanged()
        {
            if (_currentlySelectedVariable != null && VariableSelectionOrSearchConfigChanged != null)
                VariableSelectionOrSearchConfigChanged(this, _currentlySelectedVariable,
                    includeDirectAndIndirectChildVariablesCheckBox.Checked, includeRedefinesCheckBox.Checked);
        }

        private TreeNode FillVariableTreeView(Variable variable)
        {
            TreeNode newNode = VariablesUtil.Instance.ConvertToTreeNode(variable);
           
            //Saving the selected node is necessary to set it as the selected node in the VariableInfoTree later.
            //However, using a field is a little bit dirty as this method is already returning something. But the
            //only completely right way would be creating a new type like TreeNodeWithSelection which seems pretty
            //overdone so I sticked to this way.
            _selectedNode = newNode;

            //if (variable.ParentVariable != null)
            //{
            //    // save variable node
            //    var temp = newNode;
            //    var parent = variable.ParentVariable;

            //    // new node for parent variable
            //    newNode = new TreeNode(variable.ParentVariable.GetLevelAndName()) { Tag = variable.ParentVariable };

            //    //// add parents' children (siblings and self)
            //    //foreach (var sibling in variable.ParentVariable.Variables)
            //    //{
            //    //    newNode.Nodes.Add(sibling == variable
            //    //        ? temp
            //    //        : new TreeNode(sibling.GetLevelAndName()) { Tag = sibling });


            //    //}

            //    newNode.Nodes.Add(temp);

            //    // go further up, add grandparents etc.
            //    while (parent.ParentVariable != null)
            //    {
            //        newNode = new TreeNode(parent.ParentVariable.GetLevelAndName(), new[] { newNode })
            //        {
            //            Tag = parent.ParentVariable
            //        };

            //        parent = parent.ParentVariable;
            //    }
            //}

            return newNode;
        }

        private void label1_Click(object sender, EventArgs e)
        {

        }

        private void checkBox1_CheckedChanged(object sender, EventArgs e)
        {

        }

        private void checkBox2_CheckedChanged(object sender, EventArgs e)
        {

        }
    }
}
