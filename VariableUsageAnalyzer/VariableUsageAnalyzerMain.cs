using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using Model.File;

namespace VariableUsageAnalyzer
{
    public partial class VariableUsageAnalyzerMain : Form
    {
        public delegate  void VariableUsageSelectedEventHandler(object sender, Variable variable, CobolFile file, uint lineNumber);
        public event VariableUsageSelectedEventHandler VariableUsageSelected;
        public VariableUsageAnalyzerMain(Variable variable, CobolFile file)
        {
            InitializeComponent();
         

            var variableSelectionControl = new VariableSelectionControl(variable);
         variableSelectionControl.VariableSelectionOrSearchConfigChanged +=
                (sender, variable1, variables, redefines) =>
                {
                    this.splitContainer1.Panel2.Controls.Clear();
                    VariableListControl vlc = new VariableListControl(variable1, file, variables, redefines);
                    vlc.VariableUsageDoubleClicked += (o, variable2, cobolFile, number) =>
                    {
                        VariableUsageSelected(this, variable2, cobolFile, number);
                    };
                    this.splitContainer1.Panel2.Controls.Add(vlc);
                };
            this.splitContainer1.Panel1.Controls.Add(variableSelectionControl);
            this.splitContainer1.FixedPanel = FixedPanel.Panel1;
            
        }

        private void VariableUsageAnalyzer_Load(object sender, EventArgs e)
        {
            
        }

        private void splitContainer1_Panel2_Paint(object sender, PaintEventArgs e)
        {

        }
    }
}
