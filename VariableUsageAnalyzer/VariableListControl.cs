﻿using System;
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
        private bool _includeDirectAndIndirectChildVariables;
        private bool _includeRedefines;
        private Variable _variable;

        public VariableListControl(Variable variable, CobolFile file, bool includeDirectAndIndirectChildVariables, bool includeRedefines)
        {
            InitializeComponent();
            _includeDirectAndIndirectChildVariables = includeDirectAndIndirectChildVariables;
            _includeRedefines = includeRedefines;
            _variable = variable;

            Dock = DockStyle.Fill;
            tableLayoutPanel1.RowStyles.Clear();

            var lines = FindVariableInFile(_variable, file, _includeDirectAndIndirectChildVariables);

            AddContainingFileName(file.Name, numberOfChildVariables, -1, lines.Count);
            foreach (LineDto line in lines)
            {
                AddCodeLine(line);
            }

            AddEmptyBuffer();

        }



        private List<LineDto> FindVariableInFile(Variable variable, CobolFile file, bool includeChildren)
        {
            var findings = new List<LineDto>();
            var variableList = new List<Variable>();
            

            if (includeChildren)
            {
                WriteAllChildrensAndSelfIntoList(variable, variableList);
            }
            else
            {
                variableList.Add(variable);
            }
            numberOfChildVariables = variableList.Count - 1;

            using (var fileText = new StringReader(file.Text))
            {
                var currLineNumber = 1;
                var currLineText = "";
                while ((currLineText = fileText.ReadLine()) != null)
                {
                    foreach (var currVariable in variableList)
                    {
                        if (currLineText.Contains(currVariable.VariableName))
                        {
                            findings.Add(new LineDto(currLineText, currLineNumber, currVariable, file));
                            break;
                        }
                    }
                    currLineNumber++;
                }
            }
            return findings;
        }

        private void WriteAllChildrensAndSelfIntoList(Variable variable, List<Variable> variableList)
        {
            //using nameList.Contains is probably faster than casting from HashSet to List later on as the list will contains less than 10 entries in most cases
            if(!variableList.Contains(variable))
                variableList.Add(variable);
            foreach (Variable child in variable.Variables)
            {
                WriteAllChildrensAndSelfIntoList(child, variableList);
            }
        }

        public delegate void VariableUsageDoubleClickedEventHandler(object sender, Variable variable, CobolFile file, uint lineNumer);
        public event VariableUsageDoubleClickedEventHandler VariableUsageDoubleClicked;

        private void AddCodeLine(LineDto line)
        {  
            FastColoredTextBox lineBox =  new FastColoredTextBox();
            lineBox.Tag = "cl" + this.Controls.Count;
            lineBox.Name = "cl" + this.Controls.Count;
            lineBox.Dock = DockStyle.Top;
            lineBox.Height = 18;
            lineBox.Margin = new Padding(3,3,3,0);
            lineBox.Language = Language.Cobol;
            lineBox.HighlightingRangeType = HighlightingRangeType.AllTextRange;
            lineBox.Text = line.Text;

            lineBox.BorderStyle = BorderStyle.FixedSingle;
            lineBox.LineNumberStartValue = (uint) line.Number;

            lineBox.MouseDoubleClick += (sender, args) =>
            {
                VariableUsageDoubleClicked(this, line.FoundVariable, line.ContainingFile, (uint) line.Number);
            };

            AddToTable(lineBox);
            }

        private void AddContainingFileName(String filename, int childCount, int redefineCount, int usageCount)
        {
      
            var tmp = "Found " + usageCount +
                      " usage(s) of the variable " + _variable.VariableName + 
                      " in the file " + filename;
            tmp += _includeDirectAndIndirectChildVariables ? " including " + childCount + " direct or indirect children" : " not looking for direct or indirect children";
            tmp += " and";
            tmp += _includeRedefines ? " including " + redefineCount + " redefine(s)" : " not looking for redefines.";
           
            Label name = new Label();
            name.Text = tmp;
            name.Dock = DockStyle.Top;
            name.Margin = new Padding(0, 6 , 0, 0);
            name.Height = 16;
            AddToTable(name);
        }

        private void AddEmptyBuffer()
        {
            Label text = new Label();
            text.Text = " ";
            text.Height = 12;
            AddToTable(text);
        }

        private void AddToTable(Control u)
        {
            tableLayoutPanel1.Controls.Add(u);
            tableLayoutPanel1.RowCount = tableLayoutPanel1.RowCount + 1;
            tableLayoutPanel1.RowStyles.Add(new RowStyle(SizeType.AutoSize));

        }
    }
}
