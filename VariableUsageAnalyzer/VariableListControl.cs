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
        private int numberOfChildVariables;
        public VariableListControl(Variable variable, CobolFile file)
        {
            InitializeComponent();
            this.Dock = DockStyle.Fill;
            tableLayoutPanel1.RowStyles.Clear();

            var lines = FindVariableInFile(variable, file);

            AddContainingFileName("Usages of variable " + variable.VariableName + " in " +file.Name + file.FileReference.FileExtension + " including " + numberOfChildVariables + " direct or indirect child variable(s).");
            foreach (LineDto line in lines)
            {
                AddCodeLine(line);
            }

        }


        private List<LineDto> FindVariableInFile(Variable variable, CobolFile file, bool includeChildren = true)
        {
            var findings = new List<LineDto>();
            var nameList = new List<string>();
            

            if (includeChildren)
            {
                WriteAllChildrensAndOwnNamesIntoList(variable, nameList);
            }
            else
            {
                nameList.Add(variable.VariableName);
            }
            numberOfChildVariables = nameList.Count - 1;

            using (var fileText = new StringReader(file.Text))
            {
                var currLineNumber = 1;
                var currLineText = "";
                while ((currLineText = fileText.ReadLine()) != null)
                {
                    foreach (var name in nameList)
                    {
                        if (currLineText.Contains(name))
                        {
                            findings.Add(new LineDto(currLineText, currLineNumber));
                            break;
                        }
                    }
                    currLineNumber++;
                }
            }
            return findings;
        }

        private void WriteAllChildrensAndOwnNamesIntoList(Variable variable, List<string> nameList)
        {
            //using nameList.Contains is probably faster than casting from HashSet to List later on as the list will contains less than 10 entries in most cases
            if(!nameList.Contains(variable.VariableName))
                nameList.Add(variable.VariableName);
            foreach (Variable child in variable.Variables)
            {
                WriteAllChildrensAndOwnNamesIntoList(child, nameList);
            }
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
            name.Margin = new Padding(0, 6 , 0, 0);
            name.Height = 16;
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
