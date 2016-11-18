using Model.File;
using System.Windows.Forms;
using Util;

namespace VariableUsageAnalyzer
{
    public sealed partial class VariableSelectionControl : UserControl
    {
        private Variable _currentlySelectedVariable;

        public delegate void VariableSelectionOrSearchConfigChangedEventHandler(object sender, Variable variable, bool includeDirectAndIndirectChildVariables, bool includeRedefines);

        public event VariableSelectionOrSearchConfigChangedEventHandler VariableSelectionOrSearchConfigChanged;

        public VariableSelectionControl(Variable variable)
        {
            InitializeComponent();
            Dock = DockStyle.Fill;

            var treeNode = VariablesUtil.Instance.ConvertToTreeNode(variable);
            // TODO VariableTreeView is duplicate code
            VariableSelectionTreeView.SetTreeWithSelection(treeNode, treeNode);
            VariableSelectionTreeView.Dock = DockStyle.Fill;
            VariableSelectionTreeView.OnVariableSelected += (sender, variable1) => HandleVariableSelectionOrSearchConfigChanged(variable1);

            includeDirectAndIndirectChildVariablesCheckBox.CheckedChanged += (sender, args) => HandleVariableSelectionOrSearchConfigChanged();

            includeRedefinesCheckBox.CheckedChanged += (sender, args) => HandleVariableSelectionOrSearchConfigChanged();

        }

        private void HandleVariableSelectionOrSearchConfigChanged(Variable newSelection = null)
        {
            _currentlySelectedVariable = newSelection ?? _currentlySelectedVariable;

            if (_currentlySelectedVariable != null && VariableSelectionOrSearchConfigChanged != null)
                VariableSelectionOrSearchConfigChanged(this, _currentlySelectedVariable,
                    includeDirectAndIndirectChildVariablesCheckBox.Checked, includeRedefinesCheckBox.Checked);
        }
    }
}
