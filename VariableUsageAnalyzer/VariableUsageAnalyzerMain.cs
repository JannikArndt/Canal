using Model.File;
using System.Windows.Forms;
using VariableUsageAnalyzer.Properties;

namespace VariableUsageAnalyzer
{
    public sealed partial class VariableUsageAnalyzerMain : Form
    {
        private readonly CobolFile _file;

        public delegate void VariableUsageSelectedEventHandler(object sender, Variable variable, CobolFile file, string lineText);

        public event VariableUsageSelectedEventHandler VariableUsageSelected;

        public VariableUsageAnalyzerMain(Variable variable, CobolFile file)
        {
            InitializeComponent();
            Text = string.Format(Resources.VariableUsageAnalyzerWindowTitle, file.Name, variable.VariableName);

            _file = file;

            var variableSelectionControl = new VariableSelectionControl(variable);
            variableSelectionControl.VariableSelectionOrSearchConfigChanged += UpdateFindingsControl;

            splitContainer1.Panel1.Controls.Add(variableSelectionControl);
            splitContainer1.FixedPanel = FixedPanel.Panel1;

        }

        private void UpdateFindingsControl(object sender, Variable variable, bool includeDirectAndIndirectChildVariables, bool includeRedefines)
        {
            var variableListControl = new VariableListControl(variable, _file, includeDirectAndIndirectChildVariables, includeRedefines);

            variableListControl.VariableUsageDoubleClicked += (o, variable2, cobolFile, number) =>
            {
                if (VariableUsageSelected != null)
                    VariableUsageSelected(this, variable2, cobolFile, number);
            };

            splitContainer1.Panel2.Controls.Clear();
            splitContainer1.Panel2.Controls.Add(variableListControl);
        }
    }
}
