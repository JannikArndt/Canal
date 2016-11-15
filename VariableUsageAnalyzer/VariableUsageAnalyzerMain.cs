using System;
using System.Windows.Forms;
using Model.File;
using VariableUsageAnalyzer.Properties;

namespace VariableUsageAnalyzer
{
    public sealed partial class VariableUsageAnalyzerMain : Form
    {
        public delegate  void VariableUsageSelectedEventHandler(object sender, Variable variable, CobolFile file, uint lineNumber);
        public event VariableUsageSelectedEventHandler VariableUsageSelected;
        public VariableUsageAnalyzerMain(Variable variable, CobolFile file)
        {
            InitializeComponent();
            Text = string.Format(Resources.VariableUsageAnalyzerWindowTitle, file.Name, variable.VariableName);

            var variableSelectionControl = new VariableSelectionControl(variable);
            variableSelectionControl.VariableSelectionOrSearchConfigChanged +=
                (sender, variable1, variables, redefines) =>
                {
                    splitContainer1.Panel2.Controls.Clear();
                    VariableListControl vlc = new VariableListControl(variable1, file, variables, redefines);
                    vlc.VariableUsageDoubleClicked += (o, variable2, cobolFile, number) =>
                    {
                        if (VariableUsageSelected != null)
                            VariableUsageSelected(this, variable2, cobolFile, number);
                    };
                    splitContainer1.Panel2.Controls.Add(vlc);
                };
            splitContainer1.Panel1.Controls.Add(variableSelectionControl);
            splitContainer1.FixedPanel = FixedPanel.Panel1;
            
        }

      

        private void VariableUsageAnalyzer_Load(object sender, EventArgs e)
        {
            
        }

        private void splitContainer1_Panel2_Paint(object sender, PaintEventArgs e)
        {

        }
    }
}
