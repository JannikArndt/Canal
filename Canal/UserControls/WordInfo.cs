using Canal.Utils;
using Model;
using Model.References;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Windows.Forms;

namespace Canal.UserControls
{
    public partial class WordInfo : UserControl
    {
        private readonly MainWindow _parent;

        public WordInfo(string word, CobolFile cobolFile, MainWindow parent)
        {
            InitializeComponent();
            _parent = parent;

            // is the word a variable?
            var variable = cobolFile.Variables.FindVariable(word);
            if (variable != null)
            {
                infoGroupBox.Text = "Selected Variable: " + word;
                FillVariableTreeView(variable);
                return;
            }

            // is it a procedure?
            var procedure = cobolFile.CobolTree.AllProcedures.FirstOrDefault(proc => proc.Name == word);
            if (procedure != null)
            {
                infoGroupBox.Text = "Selected Procedure: " + word;
                FillProcedureTreeView(procedure);
                return;
            }

            // else: show file infos
            infoGroupBox.Text = "Program: " + cobolFile.Name;
            FillCallTreeView(cobolFile.CallReferences);
        }

        private void FillCallTreeView(List<FileReference> callReferences)
        {
            var uniqueFolders = new HashSet<string>(callReferences.Select(cr => cr.Directory));

            foreach (var folder in uniqueFolders)
            {
                var folderNode = new TreeNode(folder);
                variableTreeView.Nodes.Add(folderNode);
                var folder1 = folder;
                foreach (var fileRef in callReferences.Where(cr => cr.Directory == folder1))
                {
                    folderNode.Nodes.Add(fileRef);
                }
            }

            variableTreeView.ExpandAll();

            variableTreeView.NodeMouseDoubleClick += (sender, args) =>
            {
                var fileRef = variableTreeView.SelectedNode as FileReference;
                if (fileRef != null)
                    _parent.OpenFile(fileRef.FullPath);
            };
        }

        private void FillProcedureTreeView(Procedure procedure)
        {
            variableTreeView.Nodes.Add(procedure.Name);

            var performs = procedure.PerformReferences.Select(pref => new TreeNode(pref.ReferencedProcedure)).ToArray();
            if (performs.Any())
                variableTreeView.Nodes.Add(new TreeNode("Performs", performs));

            var gotos = procedure.GoToReferences.Select(pref => new TreeNode(pref.ReferencedProcedure)).ToArray();
            if (gotos.Any())
                variableTreeView.Nodes.Add(new TreeNode("GO TOs", gotos));

            var calls = procedure.CallReferences.Select(pref => new TreeNode(pref.ProgramName)).ToArray();
            if (calls.Any())
                variableTreeView.Nodes.Add(new TreeNode("Calls", calls));


            variableTreeView.ExpandAll();
        }

        private void FillVariableTreeView(Variable variable)
        {
            // Get all vars to the root
            var currentVar = variable;

            while (currentVar.ParentVariable != null)
            {
                currentVar.Nodes.Clear();
                foreach (var vari in currentVar.Variables)
                {
                    currentVar.Nodes.Add(vari);
                }
                currentVar = currentVar.ParentVariable;
            }

            variableTreeView.DrawMode = TreeViewDrawMode.OwnerDrawText;
            variableTreeView.DrawNode += VariableTreeViewOnDrawNode;
            variableTreeView.Nodes.Add((Variable)currentVar.Clone());
            variableTreeView.ExpandAll();
        }

        private void VariableTreeViewOnDrawNode(object sender, DrawTreeNodeEventArgs e)
        {
            var variable = e.Node as Variable;
            if (variable == null)
                return;

            Color nodeColor = Color.DarkGray;
            if ((e.State & TreeNodeStates.Selected) != 0)
                nodeColor = SystemColors.HighlightText;

            var levelWidth = 20;
            var nameWidth = 220 - e.Node.Level * 20;
            var picWidth = 120;

            TextRenderer.DrawText(e.Graphics,
                                  variable.VariableLevel.ToString("D2"),
                                  e.Node.NodeFont,
                                  new Rectangle(e.Bounds.X, e.Bounds.Y, levelWidth, e.Bounds.Height),
                                  nodeColor,
                                  Color.Empty,
                                  TextFormatFlags.VerticalCenter);

            TextRenderer.DrawText(e.Graphics,
                                  variable.VariableName,
                                  e.Node.NodeFont,
                                  new Rectangle(e.Bounds.X + levelWidth, e.Bounds.Y, nameWidth, e.Bounds.Height),
                                  e.Node.ForeColor,
                                  Color.Empty,
                                  TextFormatFlags.VerticalCenter);

            if (variable.Picture != null)
                TextRenderer.DrawText(e.Graphics,
                                      variable.Picture.ToString(),
                                      e.Node.NodeFont,
                                      new Rectangle(e.Bounds.X + levelWidth + nameWidth, e.Bounds.Y, picWidth, e.Bounds.Height),
                                      nodeColor,
                                      Color.Empty,
                                      TextFormatFlags.VerticalCenter);
        }

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            variableTreeView.Nodes.Clear();
            variableTreeView.Dispose();

            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }
    }
}
