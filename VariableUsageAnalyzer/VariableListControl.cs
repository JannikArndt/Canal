using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using FastColoredTextBoxNS;
using FastColoredTextBoxNS.Enums;

namespace VariableUsageAnalyzer
{
    public partial class VariableListControl : UserControl
    {
        private FastColoredTextBox codeBox;
        public VariableListControl(List<String> lines)
        {
            InitializeComponent();
            this.Dock = DockStyle.Fill;
            
            tableLayoutPanel1.RowStyles.Clear();
            foreach (String line in lines)
            {
                AddContainingFileName("hallo");
                AddCodeLine(line);
            }

        }

        private void AddCodeLine(String line, uint lineNumber = 12)
        {  
            FastColoredTextBox lineBox =  new FastColoredTextBox();
            lineBox.Tag = "cl" + this.Controls.Count;
            lineBox.Name = "cl" + this.Controls.Count;
            lineBox.Dock = DockStyle.Top;
            lineBox.Height = 18;
            lineBox.Text = line;
            lineBox.Margin = new Padding(3,3,3,0);
            
            lineBox.BorderStyle = BorderStyle.FixedSingle;
            lineBox.LineNumberStartValue = lineNumber;
            AddToTable(lineBox);
            
        }

        private void AddContainingFileName(String filename)
        {
            Label name = new Label();
            name.Text = filename;
            name.Dock = DockStyle.Top;
            name.Margin = new Padding(3, 10, 0, 0);
            name.Height = 12;
            AddToTable(name);
        }

        private void AddToTable(Control u)
        {
            tableLayoutPanel1.Controls.Add(u);
            tableLayoutPanel1.RowCount = tableLayoutPanel1.RowCount + 1;
            tableLayoutPanel1.RowStyles.Add(new RowStyle(SizeType.AutoSize));

        }
    }
}
