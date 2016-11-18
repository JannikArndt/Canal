using Canal.Properties;
using FastColoredTextBoxNS;
using FastColoredTextBoxNS.Enums;
using Model.File;
using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;

namespace Canal.UserControls.VariableUsageAnalyzer
{
    public sealed partial class VariableListControl : UserControl
    {
        private readonly bool _includeDirectAndIndirectChildVariables;
        private readonly bool _includeRedefines;

        public VariableListControl(Variable variable, CobolFile file, bool includeDirectAndIndirectChildVariables, bool includeRedefines)
            : this(variable, new List<CobolFile> { file }, includeDirectAndIndirectChildVariables, includeRedefines)
        {

        }

        // ReSharper disable once MemberCanBePrivate.Global
        public VariableListControl(Variable variable, IEnumerable<CobolFile> files, bool includeDirectAndIndirectChildVariables, bool includeRedefines)
        {
            InitializeComponent();
            _includeDirectAndIndirectChildVariables = includeDirectAndIndirectChildVariables;
            _includeRedefines = includeRedefines;

            Dock = DockStyle.Fill;
            tableLayoutPanel1.RowStyles.Clear();

            BuildFindingsList(files, variable);

        }

        private void BuildFindingsList(IEnumerable<CobolFile> files, Variable variable)
        {
            if (variable.VariableName == Resources.FILLER)
                AddFillerNotSupportedMessage();
            else
            {
                var variableList = BuildRelevantVariablesList(variable);
                var numberOfChildVariables = variableList.Count - 1;

                foreach (var file in files)
                {
                    var lines = FindVariablesInFile(file, variableList);
                    AddContainingFileName(file.Name, numberOfChildVariables, lines.Count, variable.VariableName);
                    foreach (LineDto line in lines)
                        AddCodeLine(line);
                }
            }

            AddEmptyBuffer();
        }


        private static List<LineDto> FindVariablesInFile(CobolFile file, List<Variable> variableList)
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

        private List<Variable> BuildRelevantVariablesList(Variable variable)
        {
            var list = new List<Variable>();
            IterateVariable(variable, list);
            return list;
        }

        private void IterateVariable(Variable variable, List<Variable> variableList)
        {
            //using nameList.Contains is probably faster than casting from HashSet to List later on as the list will contains less than 10 entries in most cases
            if (!variableList.Contains(variable))
                variableList.Add(variable);

            if (_includeDirectAndIndirectChildVariables)
                foreach (Variable child in variable.Variables)
                {
                    IterateVariable(child, variableList);
                }

            if (_includeRedefines && variable.Redefines != null)
                IterateVariable(variable.Redefines, variableList);
        }

        public delegate void VariableUsageDoubleClickedEventHandler(object sender, Variable variable, CobolFile file, string lineText);
        public event VariableUsageDoubleClickedEventHandler VariableUsageDoubleClicked;

        private void AddCodeLine(LineDto line)
        {
            var lineBox = new FastColoredTextBox
            {
                Tag = "cl" + Controls.Count,
                Name = "cl" + Controls.Count,
                Dock = DockStyle.Top,
                Height = 18,
                Margin = new Padding(3, 3, 3, 0),
                Language = Language.Cobol,
                HighlightingRangeType = HighlightingRangeType.SingleLine,
                BorderStyle = BorderStyle.FixedSingle,
                ReservedCountOfLineNumberChars = 5,
                LineNumberStartValue = (uint)line.Number
            };

            // TODO this is crazy slow
            lineBox.SetSingleLine(line.Text);

            lineBox.MouseDoubleClick += (sender, args) =>
            {
                if (VariableUsageDoubleClicked != null)
                    VariableUsageDoubleClicked(this, line.FoundVariable, line.ContainingFile, lineBox.Text);
            };

            AddToTable(lineBox);
        }

        private void AddContainingFileName(string filename, int childCount, int usageCount, string variableName)
        {

            var tmp = "Found " + usageCount +
                      " usage(s) of the variable " + variableName +
                      " in the file " + filename;
            tmp += _includeDirectAndIndirectChildVariables ? " including " + childCount + " direct or indirect children" : " not looking for direct or indirect children";
            tmp += " and";
            tmp += _includeRedefines ? " including redefines." : " not looking for redefines.";

            var name = new Label
            {
                Text = tmp,
                Dock = DockStyle.Top,
                Margin = new Padding(0, 6, 0, 0),
                Height = 16
            };

            AddToTable(name);
        }

        private void AddFillerNotSupportedMessage()
        {
            var name = new Label
            {
                Text = Resources.SearchingForFillerNotSupported,
                Dock = DockStyle.Top,
                Margin = new Padding(0, 6, 0, 0),
                Height = 16
            };

            AddToTable(name);
        }

        private void AddEmptyBuffer()
        {
            var text = new Label
            {
                Text = @" ",
                Height = 12
            };

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
