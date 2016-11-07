using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using Util;
using Model.File;

namespace VariableUsageAnalyzer
{
    public partial class VariableSelectionControl : UserControl
    {
        private TreeNode _selectedNode;

        public VariableSelectionControl(Variable variable)
        {
            InitializeComponent();
            var treeNode = FillVariableTreeView(variable);
            variableTreeView1.Dock = DockStyle.Fill;
            variableTreeView1.SetTreeWithSelection(treeNode, _selectedNode);

        }

        private TreeNode FillVariableTreeView(Variable variable)
        {
            TreeNode newNode = VariablesUtil.Instance.ConvertToTreeNode(variable);
           
            //Saving the selected node is necessary to set it as the selected node in the VariableInfoTree later.
            //However, using a field is a little bit dirty as this method is already returning something. But the
            //only completely right way would be creating a new type like TreeNodeWithSelection which seems pretty
            //overdone so I sticked to this way.
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
    }
}
