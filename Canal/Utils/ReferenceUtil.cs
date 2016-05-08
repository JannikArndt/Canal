using Logging;
using Model;
using System.ComponentModel;

namespace Canal.Utils
{
    using System;
    using System.Linq;
    using System.Windows.Forms;

    public class ReferenceUtil
    {
        public static readonly ReferenceUtil Instance = new ReferenceUtil();

        private ReferenceUtil()
        { }

        public event ProgressChangedEventHandler ProgressChanged;

        public void ResolveCopys(CobolFile file)
        {
            Logger.Info("Start resolving copys for file {0}", file.Name);

            var counter = 0;
            foreach (var copyReference in file.CopyReferences.AsParallel())
            {
                try
                {
                    counter++;
                    Logger.Info("Resolving program {0} in folder {1}", copyReference.ProgramName, copyReference.Directory);

                    TextUtil.Instance.Insert(file, copyReference);

                    if (ProgressChanged != null)
                        ProgressChanged.Invoke(null, new ProgressChangedEventArgs(counter * 100 / (file.CopyReferences.Count + 3), null));
                }
                catch (Exception exception)
                {
                    Logger.Error("Error resolving program {0} in folder {1}: {2}", copyReference.ProgramName, copyReference.Directory, exception.Message);
                }
            }

            try
            {
                var builder = new CobolTreeBuilder();
                builder.Build(file);
            }
            catch (Exception exception)
            {
                Logger.Error("Error building cobol tree: {0}", exception.Message);
            }

            if (ProgressChanged != null)
                ProgressChanged.Invoke(null, new ProgressChangedEventArgs(90, null));
        }

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
