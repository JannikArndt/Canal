using Canal.Utils;
using Model;
using System.Drawing;
using System.Linq;
using System.Windows.Forms;

namespace Canal.UserControls
{
    public partial class WordInfo : UserControl
    {
        public WordInfo(string word, FileControl fileControl)
        {
            InitializeComponent();

            // is the word a variable?
            var variable = fileControl.CobolFile.Variables.FindVariable(word);
            if (variable != null)
            {
                headingLabel.Text = word;
                FillVariableTreeView(variable);
                return;
            }

            var procedure = fileControl.CobolFile.CobolTree.AllProcedures.FirstOrDefault(proc => proc.Name == word);
            if (procedure != null)
            {
                headingLabel.Text = word;
                FillProcedureTreeView(procedure);
            }
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
            var node = new TreeNode(currentVar.VariableDefinition, currentVar.Variables.Select(child => new TreeNode(child.VariableDefinition)).ToArray());

            while (currentVar.ParentVariable != null)
            {
                var newNode = new TreeNode(currentVar.ParentVariable.VariableDefinition, new[] { node });
                node = newNode;
                currentVar = currentVar.ParentVariable;
            }

            variableTreeView.DrawMode = TreeViewDrawMode.OwnerDrawText;
            variableTreeView.DrawNode += VariableTreeViewOnDrawNode;
            variableTreeView.Nodes.Add(node);
            variableTreeView.ExpandAll();
        }

        private void VariableTreeViewOnDrawNode(object sender, DrawTreeNodeEventArgs e)
        {
            Color nodeColor = Color.DarkGray;
            if ((e.State & TreeNodeStates.Selected) != 0)
                nodeColor = SystemColors.HighlightText;

            var texts = e.Node.Text.Split('#');

            var levelWidth = 20;
            var nameWidth = 140 - e.Node.Level * 20;
            var picWidth = 80;

            TextRenderer.DrawText(e.Graphics,
                                  texts[0],
                                  SourceCodePro.Regular,
                                  new Rectangle(e.Bounds.X, e.Bounds.Y, levelWidth, e.Bounds.Height),
                                  nodeColor,
                                  Color.Empty,
                                  TextFormatFlags.VerticalCenter);

            TextRenderer.DrawText(e.Graphics,
                                  texts[1],
                                  SourceCodePro.Regular,
                                  new Rectangle(e.Bounds.X + levelWidth, e.Bounds.Y, nameWidth, e.Bounds.Height),
                                  e.Node.ForeColor,
                                  Color.Empty,
                                  TextFormatFlags.VerticalCenter);

            TextRenderer.DrawText(e.Graphics,
                                  texts[2],
                                  SourceCodePro.Regular,
                                  new Rectangle(e.Bounds.X + levelWidth + nameWidth, e.Bounds.Y, picWidth, e.Bounds.Height),
                                  nodeColor,
                                  Color.Empty,
                                  TextFormatFlags.VerticalCenter);
        }
    }
}
