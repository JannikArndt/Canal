using Model;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Windows.Forms;
using Model.File;
using Util;

namespace Canal.UserControls.WordInfoViews
{
    public partial class ProcedureInfo : UserControl
    {
        public ProcedureInfo(Procedure procedure)
        {
            InitializeComponent();

            TreeView.DrawMode = TreeViewDrawMode.OwnerDrawText;
            TreeView.DrawNode += TreeViewOnDrawNode;

            FillReferencesTreeView(procedure);
            FillReferencedByTreeView(procedure);
            FillVariableUsagesNode(procedure);

            TreeView.ExpandAll();
        }

        private void FillReferencesTreeView(Procedure procedure)
        {
            var referencesNode = new TreeNode("References") { Tag = "h1" };
            TreeView.Nodes.Add(referencesNode);

            var performs = procedure.PerformReferences.Select(pref => new TreeNode(pref.ReferencedProcedure)).ToArray();
            if (performs.Any())
                referencesNode.Nodes.Add(new TreeNode("Performs", performs) { Tag = "h1" });

            var gotos = procedure.GoToReferences.Select(pref => new TreeNode(pref.ReferencedProcedure)).ToArray();
            if (gotos.Any())
                referencesNode.Nodes.Add(new TreeNode("GO TOs", gotos) { Tag = "h1" });

            var calls = procedure.CallReferences.Select(pref => new TreeNode(pref.ProgramName)).ToArray();
            if (calls.Any())
                referencesNode.Nodes.Add(new TreeNode("Calls", calls) { Tag = "h1" });
        }

        private void FillReferencedByTreeView(Procedure procedure)
        {
            var referencedByNode = new TreeNode("Referenced By") { Tag = "h1" };
            TreeView.Nodes.Add(referencedByNode);

            foreach (var performReference in procedure.IsReferencedBy.DistinctBy(refProc => refProc.Procedure.Name))
            {
                referencedByNode.Nodes.Add(performReference.Procedure.Name);
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
                    rootVarNode.Nodes.Add(new TreeNode(variable.VariableLevel.ToString("D2") + "  " + variable.VariableName + " " + procedure.VariableUsages[variable].ToShortString()));

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
    }
}
