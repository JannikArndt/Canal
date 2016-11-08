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
        public VariableUsageAnalyzerMain(Variable variable, CobolFile file)
        {
            InitializeComponent();
         

            var variableSelectionControl = new VariableSelectionControl(variable);
            variableSelectionControl.VariableSelectionTreeView.OnVariableSelected += (sender, variable1) =>
            {
                this.splitContainer1.Panel2.Controls.Clear();
                this.splitContainer1.Panel2.Controls.Add(new VariableListControl(variable1, file));
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
