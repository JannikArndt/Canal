using FastColoredTextBoxNS.Events;
using Logging;
using Model.Exceptions;
using Model.File;
using Model.References;
using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Windows.Forms;
using Util;

namespace Canal.UserControls.WordInfoViews
{
    public partial class ProcedureInfo : UserControl
    {
        public event EventHandler<WordSelectedEventArgs> OnWordSelected;

        public ProcedureInfo(Procedure procedure)
        {
            InitializeComponent();

            TreeView.DrawMode = TreeViewDrawMode.OwnerDrawText;
            TreeView.DrawNode += TreeViewOnDrawNode;

            try
            {
                FillInformationLists(procedure);
                FillVariableUsagesNode(procedure);

                tableLayoutPanel1.RowStyles[0].Height = Math.Max(16, 13 * PerformsList.Items.Count) +6;
                tableLayoutPanel1.RowStyles[1].Height = Math.Max(16, 13 * GoTosList.Items.Count) + 6;
                tableLayoutPanel1.RowStyles[2].Height = Math.Max(16, 13 * CallsList.Items.Count) + 6;
                tableLayoutPanel1.RowStyles[3].Height = Math.Max(16, 13 * ReferencedByList.Items.Count) + 6;
            }
            catch (Exception exception)
            {
                Logger.Error("Error displaying information for Procedure {0}: {1}.", procedure.Name, exception.Message);
            }

            TreeView.NodeMouseDoubleClick += TreeViewOnNodeMouseDoubleClick;

            TreeView.ExpandAll();
        }

        private void TreeViewOnNodeMouseDoubleClick(object sender, TreeNodeMouseClickEventArgs treeNodeMouseClickEventArgs)
        {
            try
            {
                var tag = treeNodeMouseClickEventArgs.Node.Tag;

                if (tag == null || tag.ToString() == "h1" || OnWordSelected == null)
                    return;

                if (tag is ProcedureReference)
                    OnWordSelected(this, new WordSelectedEventArgs((tag as ProcedureReference).ReferencedProcedure));

                if (tag is FileReference)
                    OnWordSelected(this, new WordSelectedEventArgs((tag as FileReference).ProgramName));

                if (tag is Variable)
                    OnWordSelected(this, new WordSelectedEventArgs((tag as Variable).VariableName));
            }
            catch (Exception exception)
            {
                Logger.Error("Error on node double click. Node: {0}, Message: {1}", treeNodeMouseClickEventArgs.Node.Text, exception.Message);
            }
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

            LinesOfCodeText.Text = procedure.GetLinesOfCode() + " Lines" + Environment.NewLine;
            try
            {
                LinesOfCodeText.Text += procedure.GetLinesOfCodeRecursively() + " Lines incl. performs";
            }
            catch (RecursionTooDeepException)
            {
                LinesOfCodeText.Text += "Lines incl. performs not calculated due to recursion.";
            }

        }

        private void FillVariableUsagesNode(Procedure procedure)
        {
            var varDict = new Dictionary<Variable, List<Variable>>();

            var variablesNode = new TreeNode("Variables") { Tag = "h1" };
            TreeView.Nodes.Add(variablesNode);

            // 1. Find all root variables
            foreach (var variable in procedure.VariableUsages.Keys)
            {
                var root = variable.Root ?? variable;
                if (varDict.ContainsKey(root))
                    varDict[root].Add(variable);
                else
                    varDict.Add(root, new List<Variable> { variable });
            }

            // 2. Add all variables to their respective root
            foreach (var key in varDict.Keys.OrderBy(r => r.VariableName))
            {
                var rootVarNode = new TreeNode(key.VariableName);
                foreach (var variable in varDict[key])
                {
                    var name = variable.VariableLevel.ToString("D2") + "  " + variable.VariableName + " " +
                               procedure.VariableUsages[variable].ToShortString();
                    rootVarNode.Nodes.Add(new TreeNode(name) { Tag = variable });
                }

                variablesNode.Nodes.Add(rootVarNode);
            }
        }

        private void TreeViewOnDrawNode(object sender, DrawTreeNodeEventArgs e)
        {
            if (e.Node.Tag != null && e.Node.Tag.ToString() == "h1")
                TextRenderer.DrawText(e.Graphics,
                                      e.Node.Text,
                                      new Font(e.Node.TreeView.Font, FontStyle.Bold),
                                      new Rectangle(e.Bounds.X, e.Bounds.Y, e.Bounds.Width, e.Bounds.Height),
                                      Color.Black,
                                      Color.Empty,
                                      TextFormatFlags.VerticalCenter);
            // else if (e.Node.Tag is Variable) TODO implement including VariableUsages
            //    VariableInfo.VariableInfoTreeViewOnDrawNode(sender, e);
            else
                TextRenderer.DrawText(e.Graphics,
                                          e.Node.Text,
                                          e.Node.TreeView.Font,
                                          new Rectangle(e.Bounds.X, e.Bounds.Y, e.Bounds.Width, e.Bounds.Height),
                                          Color.Black,
                                          Color.Empty,
                                          TextFormatFlags.VerticalCenter);
        }

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            TreeView.Nodes.Clear();
            TreeView.Dispose();

            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        private void SelectedIndexChanged(object sender, EventArgs e)
        {

        }

        private void PerformsList_DoubleClick(object sender, EventArgs e)
        {
            if (OnWordSelected != null && ((ListBox)sender).SelectedItem != null)
            {
                OnWordSelected(this, new WordSelectedEventArgs(((ListBox)sender).SelectedItem.ToString()));
                ((ListBox)sender).ClearSelected();
            }
        }
    }
}
