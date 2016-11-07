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
        public VariableUsageAnalyzerMain(Variable variable = null)
        {
            InitializeComponent();
            List<String> lines = new List<string>();
            lines.AddRange(new [] { "040300     MOVE    SPACES                      TO BETEI-FEHLER   ", "zwei", "drei", "vier"});
            this.splitContainer1.Panel2.Controls.Add(new VariableListControl(lines));

            this.splitContainer1.Panel1.Controls.Add(new VariableSelectionControl(variable));
        }

        private void VariableUsageAnalyzer_Load(object sender, EventArgs e)
        {
            
        }

        private void splitContainer1_Panel2_Paint(object sender, PaintEventArgs e)
        {

        }
    }
}
