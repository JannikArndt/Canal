using Logging;
using Model;

namespace Canal.Utils
{
    using System.Linq;
    using System.Windows.Forms;

    public class ReferenceUtil
    {
        public static readonly ReferenceUtil Instance = new ReferenceUtil();

        private ReferenceUtil()
        { }

        public TreeNode GetPerformTree(CobolFile file)
        {
            if (file == null)
                return null;

            Logger.Info("Creating perform tree for file {0}", file.Name);

            if (file.CobolTree == null || file.CobolTree.ProcedureDivision == null)
                return new TreeNode();

            var topNode = new TreeNode(file.Name);

            var sections = file.CobolTree.ProcedureDivision.Sections;

            // for all top-level-sections
            foreach (var section in sections.Where(sec => !sec.IsReferencedBy.Any()))
            {
                var node = new TreeNode(section.Name);

                foreach (var procedure in section.Procedures)
                {
                    FindPerformsRecursively(node, procedure);
                }

                topNode.Nodes.Add(node);
            }

            return topNode;
        }

        private void FindPerformsRecursively(TreeNode topNode, Procedure procedure, int depth = 5)
        {
            Logger.Info("Finding performs recursively for node {0}", topNode.Text);

            if (procedure == null)
                return;

            if (depth == 0)
            {
                topNode.Nodes.Add("Max. recursive depth reached.");
                return;
            }

            var tempNode = topNode.Parent;

            // find circles
            while (tempNode != null && tempNode.Parent != null)
            {
                if (tempNode.Text == procedure.Name)
                    return;
                tempNode = tempNode.Parent;
            }

            // create nodes for perform references and continue recusively
            foreach (var performReference in procedure.PerformReferences)
            {
                var newNode = new TreeNode(performReference.ReferencedProcedure);
                topNode.Nodes.Add(newNode);
                FindPerformsRecursively(newNode, performReference.Procedure, depth - 1);
            }
        }
    }
}
