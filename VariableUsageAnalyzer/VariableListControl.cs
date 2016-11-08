using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using FastColoredTextBoxNS;
using FastColoredTextBoxNS.Enums;
using Model.File;

namespace VariableUsageAnalyzer
{
    public partial class VariableListControl : UserControl
    {
        private FastColoredTextBox codeBox;
        public VariableListControl(Variable variable, CobolFile file)
        {
            InitializeComponent();
            this.Dock = DockStyle.Fill;
            tableLayoutPanel1.RowStyles.Clear();

            var lines = FindVariableInFile(variable, file);

            AddContainingFileName(file.Name);
            foreach (LineDto line in lines)
            {
                AddCodeLine(line);
            }

        }


        private List<LineDto> FindVariableInFile(Variable variable, CobolFile file)
        {
            var findings = new List<LineDto>();
            var currLineNumber = 1;
            var currLineText = "";
            using (var fileText = new StringReader(file.Text))
            {
                while ((currLineText = fileText.ReadLine()) != null)
                {
                    if (currLineText.Contains(variable.VariableName))
                    {
                        findings.Add(new LineDto(currLineText, currLineNumber));
                    }
                    currLineNumber++;
                }
            }
            return findings;
        }

        private void AddCodeLine(LineDto line)
        {  
            FastColoredTextBox lineBox =  new FastColoredTextBox();
            lineBox.Tag = "cl" + this.Controls.Count;
            lineBox.Name = "cl" + this.Controls.Count;
            lineBox.Dock = DockStyle.Top;
            lineBox.Height = 18;
            lineBox.Text = line.Text;
            lineBox.Margin = new Padding(3,3,3,0);
            
            lineBox.BorderStyle = BorderStyle.FixedSingle;
            lineBox.LineNumberStartValue = (uint) line.Number;
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
