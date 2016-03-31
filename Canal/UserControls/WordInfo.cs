using Canal.Utils;
using Model;
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
            var node = new TreeNode(currentVar.VariableName, currentVar.Variables.Select(child => new TreeNode(child.VariableName)).ToArray());

            while (currentVar.ParentVariable != null)
            {
                var newNode = new TreeNode(currentVar.ParentVariable.VariableName, new[] { node });
                node = newNode;
                currentVar = currentVar.ParentVariable;
            }

            variableTreeView.Nodes.Add(node);
            variableTreeView.ExpandAll();
        }
    }
}
