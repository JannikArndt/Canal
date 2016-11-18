using Canal.Properties;
using FastColoredTextBoxNS.Events;
using Logging;
using Model.Exceptions;
using Model.File;
using Model.Pictures;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;
using Util;

namespace Canal.UserControls.WordInfoViews
{
    public partial class ProcedureInfo : UserControl
    {
        public event EventHandler<WordSelectedEventArgs> OnProcedureSelected;

        private Procedure CurrentProcedure { get; set; }

        private readonly FileControl _parent;

        public ProcedureInfo(Procedure procedure, FileControl fileControl)
        {
            InitializeComponent();

            _parent = fileControl;

            Logger.Info("Showing info for procedure {0}.", procedure.Name);

            CurrentProcedure = procedure;

            try
            {
                FillInformationLists(procedure);
                var variables = FillVariableUsagesNode(procedure);
                variableTreeView.SetTree(variables);

                tableLayoutPanel1.RowStyles[0].Height = Math.Max(16, 13 * PerformsList.Items.Count) + 6;
                tableLayoutPanel1.RowStyles[1].Height = Math.Max(16, 13 * GoTosList.Items.Count) + 6;
                tableLayoutPanel1.RowStyles[2].Height = Math.Max(16, 13 * CallsList.Items.Count) + 6;
                tableLayoutPanel1.RowStyles[3].Height = Math.Max(16, 13 * ReferencedByList.Items.Count) + 6;
            }
            catch (Exception exception)
            {
                Logger.Error("Error displaying information for Procedure {0}: {1}.", procedure.Name, exception.Message);
            }

            variableTreeView.OnVariableDoubleClicked += (sender, clickedVariable) =>
            {
                if (clickedVariable.Root != null && clickedVariable.Root.CopyReference != null)
                    _parent.MainWindow.OpenFile(clickedVariable.Root.CopyReference.FilePath, clickedVariable);
            };
        }

        private void FillInformationLists(Procedure procedure)
        {
            foreach (var perform in procedure.PerformReferences.Select(pref => pref.ReferencedProcedure))
            {
                PerformsList.Items.Add(perform);
            }

            foreach (var perform in procedure.GoToReferences.Select(pref => pref.ReferencedProcedure))
            {
                GoTosList.Items.Add(perform);
            }

            foreach (var perform in procedure.CallReferences.Select(pref => pref.ProgramName))
            {
                CallsList.Items.Add(perform);
            }

            foreach (var perform in procedure.IsReferencedBy.DistinctBy(refProc => refProc.Procedure.Name).Select(pref => pref.ReferencedProcedure))
            {
                ReferencedByList.Items.Add(perform);
            }

            LinesOfCodeText.Text = string.Format("{0} Lines", procedure.GetLinesOfCode()) + Environment.NewLine;

            try
            {
                LinesOfCodeText.Text += string.Format("{0} Lines incl. performs", procedure.GetLinesOfCodeRecursively());
            }
            catch (RecursionTooDeepException)
            {
                LinesOfCodeText.Text += Resources.ProcedureInfo_LinesOfCodeError;
            }
        }

        private List<TreeNode> FillVariableUsagesNode(Procedure procedure)
        {
            var varDict = new Dictionary<Variable, List<Variable>>();

            var result = new List<TreeNode>();
            var local = new Variable(0, "Working-Storage Section", new PicGroup(), null);

            // 1. Find all root variables
            foreach (var variable in procedure.VariableUsages.Keys)
            {
                var root = variable.Root ?? local;
                if (varDict.ContainsKey(root))
                    varDict[root].Add(variable);
                else
                    varDict.Add(root, new List<Variable> { variable });
            }

            // 2. Add all variables to their respective root
            foreach (var key in varDict.Keys.OrderBy(r => r.VariableName))
            {
                var rootVarNode = new TreeNode(key.VariableName) { Tag = key };
                foreach (var variable in varDict[key])
                {
                    rootVarNode.Nodes.Add(new TreeNode(variable.GetLevelAndName()) { Tag = variable });
                }

                result.Add(rootVarNode);
            }

            return result;
        }

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            variableTreeView.Dispose();

            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        private void ProcedureList_DoubleClick(object sender, EventArgs e)
        {
            if (OnProcedureSelected == null || ((ListBox)sender).SelectedItem == null) return;

            var clickedProcedureName = ((ListBox)sender).SelectedItem.ToString();
            var lookFor = sender == ReferencedByList ? CurrentProcedure.Name : "";

            OnProcedureSelected(this, new WordSelectedEventArgs(clickedProcedureName, lookFor));
            ((ListBox)sender).ClearSelected();
        }
    }
}
