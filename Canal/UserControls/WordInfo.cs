using Canal.Properties;
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
        private readonly FileControl _parent;

        public WordInfo(string word, CobolFile cobolFile, FileControl parent)
        {
            InitializeComponent();
            _parent = parent;

            // is the word a variable?
            var variable = cobolFile.CobolTree.DataDivision.Variables.FindVariable(word);
            if (variable != null)
            {
                infoLabel.Visible = false;
                infoGroupBox.Text = Resources.SelectedVariable + word;
                FillVariableTreeView(variable);
                return;
            }

            // is it a procedure?
            var procedure = cobolFile.CobolTree.AllProcedures.FirstOrDefault(proc => proc.Name == word);
            if (procedure != null)
            {
                infoLabel.Visible = false;
                infoGroupBox.Text = Resources.SelectedProcedure + word;
                FillProcedureTreeView(procedure);
                return;
            }

            // else: show file infos
            infoLabel.Visible = true;
            infoGroupBox.Text = Resources.SelectedProgram + cobolFile.Name;
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
                    _parent.MainWindow.OpenFile(fileRef.FilePath);
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

            if (currentVar.ParentVariable != null)
            {
                // Add siblings without their children (which unfortunately deletes own children)
                currentVar.ParentVariable.Nodes.Clear();
                foreach (var vari in currentVar.ParentVariable.Variables)
                {
                    vari.Nodes.Clear();
                    currentVar.ParentVariable.Nodes.Add(vari);
                }

                // re-add own children
                foreach (var vari in currentVar.Variables)
                {
                    currentVar.Nodes.Add(vari);
                }

                // continue with parent
                currentVar = currentVar.ParentVariable;
            }

            // now add only grandparents, not their siblings or children
            while (currentVar.ParentVariable != null)
            {
                currentVar.ParentVariable.Nodes.Clear();
                currentVar.ParentVariable.Nodes.Add(currentVar);
                currentVar = currentVar.ParentVariable;
            }

            variableTreeView.DrawMode = TreeViewDrawMode.OwnerDrawText;
            variableTreeView.DrawNode += VariableTreeViewOnDrawNode;
            variableTreeView.Nodes.Add((Variable)currentVar.Clone());

            variableTreeView.ExpandAll();

            if (currentVar.CopyReference != null)
            {
                gotoFileButton.Visible = true;
                gotoFileButton.Click += (sender, args) => _parent.MainWindow.OpenFile(currentVar.CopyReference.FilePath);
            }

            variableTreeView.NodeMouseDoubleClick += (sender, args) =>
            {
                var vari = variableTreeView.SelectedNode as Variable;
                if (vari != null)
                    _parent.FindInCodeBox(vari.VariableName, false, false, false, true);
            };
        }

        private void VariableTreeViewOnDrawNode(object sender, DrawTreeNodeEventArgs e)
        {
            var variable = e.Node as Variable;
            if (variable == null)
                return;

            var levelWidth = 20;
            var nameWidth = 280 - e.Node.Level * 20;
            var picWidth = 160;

            // Level
            TextRenderer.DrawText(e.Graphics,
                                  variable.VariableLevel.ToString("D2"),
                                  e.Node.NodeFont,
                                  new Rectangle(e.Bounds.X, e.Bounds.Y, levelWidth, e.Bounds.Height),
                                  Color.DarkGray,
                                  Color.Empty,
                                  TextFormatFlags.VerticalCenter);

            // Name
            TextRenderer.DrawText(e.Graphics,
                                  variable.VariableName,
                                  e.Node.NodeFont,
                                  new Rectangle(e.Bounds.X + levelWidth, e.Bounds.Y, nameWidth, e.Bounds.Height),
                                  (e.State & TreeNodeStates.Selected) != 0 ? SystemColors.HighlightText : e.Node.ForeColor,
                                  Color.Empty,
                                  TextFormatFlags.VerticalCenter);

            // PIC
            if (variable.Picture != null)
                TextRenderer.DrawText(e.Graphics,
                                      variable.Picture.ToString(),
                                      e.Node.NodeFont,
                                      new Rectangle(e.Bounds.X + levelWidth + nameWidth, e.Bounds.Y, picWidth, e.Bounds.Height),
                                      Color.DarkGray,
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
