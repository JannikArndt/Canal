using Logging;
using Model;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Windows.Forms;

namespace Util
{
    public class TableOfContentsWorker
    {
        private readonly CobolFile _cobolFile;

        private TreeNode _performsTree;

        public TableOfContentsWorker(CobolFile cobolFile)
        {
            _cobolFile = cobolFile;

            if (_cobolFile.CobolTree == null)
            {
                Logger.Error("PerformTree could not be built because CobolTree is null.");
                return;
            }

            var performTreeWorker = new BackgroundWorker();

            performTreeWorker.DoWork += delegate (object sender, DoWorkEventArgs args)
            {
                args.Result = GetPerformTree(_cobolFile);
            };

            performTreeWorker.RunWorkerCompleted += delegate (object sender, RunWorkerCompletedEventArgs args)
            {
                _performsTree = (TreeNode)args.Result;
            };

            performTreeWorker.RunWorkerAsync();
        }


        public TreeNode GetToc(SortKind tocSort, string query)
        {
            switch (tocSort)
            {
                case SortKind.Alphabetical:
                    return ConvertToFlatToc(_cobolFile.CobolTree, _cobolFile.Name, query);
                case SortKind.BySections:
                    return ConvertToTreeNodes(_cobolFile.CobolTree, _cobolFile.Name, query);
                case SortKind.ByPerforms:
                    if (_performsTree == null)
                        return new TreeNode("Performs-Tree is currently being built...");
                    return _performsTree;
                default:
                    throw new ArgumentOutOfRangeException();
            }
        }


        #region Sort Alphabetical

        private static TreeNode ConvertToFlatToc(CobolTree cobolTree, string name, string query = "")
        {
            var result = new TreeNode(name);

            AddFlattenedOrderedIfNotNull(result, cobolTree.IdentificationDivision, query);
            AddFlattenedOrderedIfNotNull(result, cobolTree.EnvironmentDivision, query);
            AddFlattenedOrderedIfNotNull(result, cobolTree.DataDivision, query);
            AddFlattenedOrderedIfNotNull(result, cobolTree.ProcedureDivision, query);

            return result;
        }

        private static void AddFlattenedOrderedIfNotNull(TreeNode result, CobolTreeNode parent, string query = "")
        {
            if (parent == null)
                return;

            var orderedFlatNode = new TreeNode(parent.Name, Flatten(parent, true, query).OrderBy(node => node.Text).ToArray());
            result.Nodes.Add(orderedFlatNode);
        }

        private static IEnumerable<TreeNode> Flatten(CobolTreeNode parent, bool ignoreFirst = false, string query = "")
        {
            if (!ignoreFirst)
                yield return new TreeNode(parent.Name);

            foreach (var child in parent.GetNodes()) // check null if you must
                foreach (var relative in Flatten(child, query: query))
                    if (string.IsNullOrWhiteSpace(query) || relative.Text.IndexOf(query, StringComparison.OrdinalIgnoreCase) > -1)
                        yield return new TreeNode(relative.Text);
        }

        #endregion

        #region Sort By Section

        private static TreeNode ConvertToTreeNodes(CobolTree cobolTree, string name, string query = "")
        {
            var result = new TreeNode(name);
            if (cobolTree.IdentificationDivision != null)
                result.Nodes.Add(ConvertToTreeNode(cobolTree.IdentificationDivision, query));

            if (cobolTree.EnvironmentDivision != null)
                result.Nodes.Add(ConvertToTreeNode(cobolTree.EnvironmentDivision, query));

            if (cobolTree.DataDivision != null)
                result.Nodes.Add(ConvertToTreeNode(cobolTree.DataDivision, query));

            if (cobolTree.ProcedureDivision != null)
                result.Nodes.Add(ConvertToTreeNode(cobolTree.ProcedureDivision, query));
            return result;
        }

        private static TreeNode ConvertToTreeNode(CobolTreeNode cobolTreeNode, string query = "")
        {
            var result = new TreeNode(cobolTreeNode.Name);
            foreach (var treeNode in cobolTreeNode.GetNodes())
            {
                var node = ConvertToTreeNode(treeNode, query);
                // if query is empty, match or Nodes contains match
                if (string.IsNullOrWhiteSpace(query) || node.Text.IndexOf(query, StringComparison.OrdinalIgnoreCase) > -1 || node.Nodes.Count > 0)
                    result.Nodes.Add(node);
            }

            return result;
        }

        #endregion

        #region Sort by Perform

        private TreeNode GetPerformTree(CobolFile file)
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

        #endregion
    }
}
