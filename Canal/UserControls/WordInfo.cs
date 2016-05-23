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

            if (cobolFile == null)
                return;

            if (cobolFile.CobolTree != null)
            {
                // is the word a variable?
                if (cobolFile.Variables.ContainsKey(word))
                {
                    infoLabel.Visible = false;
                    infoGroupBox.Text = Resources.SelectedVariable + word;
                    FillVariableTreeView(cobolFile.Variables[word]);
                    return;
                }

                // is it a procedure?
                var procedure = cobolFile.CobolTree.GetAllProcedures().FirstOrDefault(proc => proc.Name == word);
                if (procedure != null)
                {
                    infoLabel.Visible = false;
                    infoGroupBox.Text = Resources.SelectedProcedure + word;
                    FillProcedureTreeView(procedure);
                    return;
                }
            }

            // else: show file infos
            infoLabel.Visible = true;
            infoGroupBox.Text = Resources.SelectedProgram + cobolFile.Name;
            FillCallTreeView(cobolFile);
        }

        private void FillCallTreeView(CobolFile cobolFile)
        {
            if (cobolFile.CobolTree == null)
            {
                return;
            }

            var uniqueFolders = new HashSet<string>(cobolFile.CobolTree.CallReferences.Select(cr => cr.Directory));

            foreach (var folder in uniqueFolders)
            {
                var folderNode = new TreeNode(folder);
                variableTreeView.Nodes.Add(folderNode);
                var folder1 = folder;
                foreach (var fileRef in cobolFile.CobolTree.CallReferences.Where(cr => cr.Directory == folder1))
                {
                    folderNode.Nodes.Add(new TreeNode(fileRef.ProgramName) { Tag = fileRef });
                }
            }

            variableTreeView.ExpandAll();

            variableTreeView.NodeMouseDoubleClick += (sender, args) =>
            {
                var fileRef = variableTreeView.SelectedNode.Tag as FileReference;
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
            var newNode = VariablesUtil.Instance.ConvertToTreeNode(variable);

            var parent = variable;

            if (variable.ParentVariable != null)
            {
                // save variable node
                var temp = newNode;

                // new node for parent variable
                newNode = new TreeNode(variable.ParentVariable.GetLevelAndName()) { Tag = variable.ParentVariable };

                // add parents' children (siblings and self)
                foreach (var sibling in variable.ParentVariable.Variables)
                {
                    newNode.Nodes.Add(sibling == variable
                        ? temp
                        : new TreeNode(sibling.GetLevelAndName()) { Tag = sibling });
                }

                // go further up, add grandparents etc.
                while (parent.ParentVariable != null)
                {
                    newNode = new TreeNode(parent.ParentVariable.GetLevelAndName(), new[] { newNode })
                    {
                        Tag = variable.ParentVariable
                    };
                    parent = parent.ParentVariable;
                }
            }

            variableTreeView.DrawMode = TreeViewDrawMode.OwnerDrawText;
            variableTreeView.DrawNode += VariableTreeViewOnDrawNode;
            variableTreeView.Nodes.Add(newNode);
            variableTreeView.ExpandAll();

            if (parent.CopyReference != null)
            {
                gotoFileButton.Visible = true;
                gotoFileButton.Click += (sender, args) => _parent.MainWindow.OpenFile(parent.CopyReference.FilePath, variable);
            }

            variableTreeView.NodeMouseDoubleClick += (sender, args) =>
            {
                var vari = variableTreeView.SelectedNode.Tag as Variable;
                if (vari != null)
                    _parent.FindInCodeBox(vari.VariableName, false, false, false, true);
            };
        }

        private void VariableTreeViewOnDrawNode(object sender, DrawTreeNodeEventArgs e)
        {
            var variable = e.Node.Tag as Variable;
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
