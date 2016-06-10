using Canal.Properties;
using Model;
using Model.References;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Windows.Forms;
using Util;

namespace Canal.UserControls
{
    public partial class WordInfo : UserControl
    {
        private readonly FileControl _parent;

        public WordInfo(string word, CobolFile cobolFile, FileControl parent)
        {
            InitializeComponent();
            _parent = parent;

            // 1. No CobolFile? Nothing to do.
            if (cobolFile == null)
                return;

            // 2. No CobolTree? Show Program infos
            if (cobolFile.CobolTree == null)
            {
                // else: show file infos
                infoGroupBox.Text = Resources.SelectedProgram + cobolFile.Name;
                FillCallTreeView(cobolFile);
                return;
            }

            // 3. Is the word a variable?
            if (cobolFile.Variables.ContainsKey(word))
            {
                infoGroupBox.Text = Resources.SelectedVariable + word;
                FillVariableTreeView(cobolFile.Variables[word]);
                return;
            }

            // 4. Is the word a procedure?
            var procedure = cobolFile.CobolTree.GetAllProcedures().FirstOrDefault(proc => proc.Name == word);
            if (procedure != null)
            {
                infoGroupBox.Text = Resources.SelectedProcedure + word;
                FillProcedureTreeView(procedure);
                return;
            }
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
                wordInfoTreeView.Nodes.Add(folderNode);
                var folder1 = folder;
                foreach (var fileRef in cobolFile.CobolTree.CallReferences.Where(cr => cr.Directory == folder1))
                {
                    folderNode.Nodes.Add(new TreeNode(fileRef.ProgramName) { Tag = fileRef });
                }
            }

            wordInfoTreeView.ExpandAll();

            wordInfoTreeView.NodeMouseDoubleClick += (sender, args) =>
            {
                var fileRef = wordInfoTreeView.SelectedNode.Tag as FileReference;
                if (fileRef != null)
                    _parent.MainWindow.OpenFile(fileRef.FilePath);
            };
        }

        private void FillProcedureTreeView(Procedure procedure)
        {
            wordInfoTreeView.Nodes.Add(procedure.Name);

            var performs = procedure.PerformReferences.Select(pref => new TreeNode(pref.ReferencedProcedure)).ToArray();
            if (performs.Any())
                wordInfoTreeView.Nodes.Add(new TreeNode("Performs", performs));

            var gotos = procedure.GoToReferences.Select(pref => new TreeNode(pref.ReferencedProcedure)).ToArray();
            if (gotos.Any())
                wordInfoTreeView.Nodes.Add(new TreeNode("GO TOs", gotos));

            var calls = procedure.CallReferences.Select(pref => new TreeNode(pref.ProgramName)).ToArray();
            if (calls.Any())
                wordInfoTreeView.Nodes.Add(new TreeNode("Calls", calls));

            // Variable usages
            wordInfoTreeView.Nodes.Add(GetVariableUsagesNode(procedure));

            wordInfoTreeView.ExpandAll();
        }

        private TreeNode GetVariableUsagesNode(Procedure procedure)
        {
            var result = new TreeNode("Variables");
            var varDict = new Dictionary<Variable, List<Variable>>();

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

                result.Nodes.Add(rootVarNode);
            }

            return result;
        }

        private void FillVariableTreeView(Variable variable)
        {
            var newNode = VariablesUtil.Instance.ConvertToTreeNode(variable);

            var parent = variable;

            if (variable.ParentVariable != null)
            {
                // save variable node
                var temp = newNode;
                parent = variable.ParentVariable;

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
                        Tag = parent.ParentVariable
                    };

                    parent = parent.ParentVariable;
                }
            }

            wordInfoTreeView.DrawMode = TreeViewDrawMode.OwnerDrawText;
            wordInfoTreeView.DrawNode += WordInfoTreeViewOnDrawNode;
            wordInfoTreeView.Nodes.Add(newNode);
            wordInfoTreeView.ExpandAll();

            if (parent.CopyReference != null)
            {
                gotoFileButton.Visible = true;
                gotoFileButton.Click += (sender, args) => _parent.MainWindow.OpenFile(parent.CopyReference.FilePath, variable);
            }

            wordInfoTreeView.NodeMouseDoubleClick += (sender, args) =>
            {
                var vari = wordInfoTreeView.SelectedNode.Tag as Variable;
                if (vari != null)
                    _parent.FindInCodeBox(vari.VariableName, false, false, false, true);
            };
        }

        private void WordInfoTreeViewOnDrawNode(object sender, DrawTreeNodeEventArgs e)
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
            wordInfoTreeView.Nodes.Clear();
            wordInfoTreeView.Dispose();

            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }
    }
}
