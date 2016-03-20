using Canal.Utils;
using System.Linq;
using System.Windows.Forms;

namespace Canal.UserControls
{
    public partial class WordInfo : UserControl
    {
        public string Word { get; set; }

        public FileControl parent { get; set; }

        public WordInfo(string word, FileControl fileControl)
        {
            InitializeComponent();
            headingLabel.Text = word;
            parent = fileControl;
            FillVariableTreeView(word);
        }

        private void FillVariableTreeView(string word)
        {
            var variable = parent.CobolFile.Variables.FindVariable(word);
            if (variable == null)
            {
                variableTreeView.Nodes.Add(new TreeNode("No data found - is this variable in a copy book?"));
                variableTreeView.ExpandAll();
                return;
            }

            // Get all vars to the root
            var currentVar = variable;
            var node = new TreeNode(currentVar.Name, currentVar.Variables.Select(child => new TreeNode(child.Name)).ToArray());

            while (currentVar.Parent != null)
            {
                var newNode = new TreeNode(currentVar.Parent.Name, new[] { node });
                node = newNode;
                currentVar = currentVar.Parent;
            }


            variableTreeView.Nodes.Add(node);
            variableTreeView.ExpandAll();
        }
    }
}
