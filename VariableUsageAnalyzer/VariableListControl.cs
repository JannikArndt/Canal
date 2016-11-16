using System;
using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;
using FastColoredTextBoxNS;
using FastColoredTextBoxNS.Enums;
using Model.File;
using VariableUsageAnalyzer.Properties;

namespace VariableUsageAnalyzer
{
    public sealed partial class VariableListControl : UserControl
    {
        private readonly bool _includeDirectAndIndirectChildVariables;
        private readonly bool _includeRedefines;
        private readonly Variable _variable;

        public VariableListControl(Variable variable, CobolFile file, bool includeDirectAndIndirectChildVariables, bool includeRedefines) : this(variable, new List<CobolFile> {file}, includeDirectAndIndirectChildVariables, includeRedefines)
        {
          
        }

        public VariableListControl(Variable variable, List<CobolFile> files, bool includeDirectAndIndirectChildVariables, bool includeRedefines)
        {
            InitializeComponent();
            _includeDirectAndIndirectChildVariables = includeDirectAndIndirectChildVariables;
            _includeRedefines = includeRedefines;
            _variable = variable;

            Dock = DockStyle.Fill;
            tableLayoutPanel1.RowStyles.Clear();

            BuildFindingsList(files);
            
        }

        private void BuildFindingsList(List<CobolFile> files)
        {
            if (_variable.VariableName == "FILLER")
                AddFillerNotSupportedMessage();
            else {
                var variableList = new List<Variable>();
                if (_includeDirectAndIndirectChildVariables)
                    WriteAllChildrensAndSelfIntoList(_variable, variableList);
                else
                    variableList.Add(_variable);
                var numberOfChildVariables = variableList.Count - 1;

                foreach (var file in files)
                {
                    var lines = FindVariablesInFile(file, variableList);
                    AddContainingFileName(file.Name, numberOfChildVariables, -1, lines.Count);
                    foreach (LineDto line in lines)
                        AddCodeLine(line);
               }
            }
            AddEmptyBuffer();
        }


        private List<LineDto> FindVariablesInFile(CobolFile file, List<Variable> variableList )
        {
            var findings = new List<LineDto>();
            using (var fileText = new StringReader(file.Text))
            {
                var currLineNumber = 1;
                string currLineText;
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

        public delegate void VariableUsageDoubleClickedEventHandler(object sender, Variable variable, CobolFile file, string lineText);
        public event VariableUsageDoubleClickedEventHandler VariableUsageDoubleClicked;

        private void AddCodeLine(LineDto line)
        {  
            FastColoredTextBox lineBox =  new FastColoredTextBox();
            lineBox.Tag = "cl" + Controls.Count;
            lineBox.Name = "cl" + Controls.Count;
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
                if (VariableUsageDoubleClicked != null)
                    VariableUsageDoubleClicked(this, line.FoundVariable, line.ContainingFile, lineBox.Text);
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

        private void AddFillerNotSupportedMessage()
        {
            Label name = new Label();
            name.Text = Resources.SearchingForFillerNotSupported;
            name.Dock = DockStyle.Top;
            name.Margin = new Padding(0, 6, 0, 0);
            name.Height = 16;
            AddToTable(name);
        }

        private void AddEmptyBuffer()
        {
            Label text = new Label();
            text.Text = @" ";
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
