using Logging;
using Model.File;
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

        public event EventHandler<RunWorkerCompletedEventArgs> PerformTreeIsBuilt;

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
                args.Result = GetPerformTree();
            };

            performTreeWorker.RunWorkerCompleted += delegate (object sender, RunWorkerCompletedEventArgs args)
            {
                _performsTree = (TreeNode)args.Result;
                if (PerformTreeIsBuilt != null)
                    PerformTreeIsBuilt(this, args);
            };

            performTreeWorker.RunWorkerAsync();
        }


        public TreeNode GetToc(SortKind tocSort, string query)
        {
            switch (tocSort)
            {
                case SortKind.Alphabetical:
                    return ConvertToFlatToc(query);
                case SortKind.BySections:
                    return ConvertToTreeNodes(query);
                case SortKind.ByPerforms:
                    if (string.IsNullOrEmpty(query))
                        return _performsTree;
                    return FilterByQuery(_performsTree, query) ?? new TreeNode();
                default:
                    throw new ArgumentOutOfRangeException();
            }
        }


        #region Sort Alphabetical

        private TreeNode ConvertToFlatToc(string query = "")
        {
            var result = new TreeNode(_cobolFile.Name);

            AddFlattenedOrderedIfNotNull(result, _cobolFile.CobolTree.IdentificationDivision, query);
            AddFlattenedOrderedIfNotNull(result, _cobolFile.CobolTree.EnvironmentDivision, query);
            AddFlattenedOrderedIfNotNull(result, _cobolFile.CobolTree.DataDivision, query);
            AddFlattenedOrderedIfNotNull(result, _cobolFile.CobolTree.ProcedureDivision, query);

            if (result.Nodes.Count == 0 && _cobolFile.Variables.Count > 0)
                result.Nodes.AddRange(_cobolFile.GetLocalRootVariables()
                        .Select(variable => new TreeNode(variable.VariableName) { Tag = variable })
                        .OrderBy(node => node.Text)
                        .ToArray());

            return result;
        }

        private void AddFlattenedOrderedIfNotNull(TreeNode result, CobolTreeNode parent, string query = "")
        {
            if (parent == null || parent.Length == 0)
                return;

            var orderedFlatNode = new TreeNode(parent.Name, Flatten(parent, true, query).OrderBy(node => node.Text).ToArray());
            result.Nodes.Add(orderedFlatNode);
        }

        private IEnumerable<TreeNode> Flatten(CobolTreeNode parent, bool ignoreFirst = false, string query = "")
        {
            if (!ignoreFirst)
                yield return new TreeNode(parent.Name);

            foreach (var child in parent.GetNodes()) // check null if you must
                foreach (var relative in Flatten(child, query: query))
                    if ((string.IsNullOrWhiteSpace(query) || relative.Text.ContainsIgnoreCase(query)) && child.Name != relative.Name)
                        yield return new TreeNode(relative.Text);
        }

        #endregion

        #region Sort By Section

        private TreeNode ConvertToTreeNodes(string query = "")
        {
            var result = new TreeNode(_cobolFile.Name);
            if (_cobolFile.CobolTree.IdentificationDivision != null && _cobolFile.CobolTree.IdentificationDivision.Length > 0)
                result.Nodes.Add(ConvertToTreeNode(_cobolFile.CobolTree.IdentificationDivision, query));

            if (_cobolFile.CobolTree.EnvironmentDivision != null && _cobolFile.CobolTree.EnvironmentDivision.Length > 0)
                result.Nodes.Add(ConvertToTreeNode(_cobolFile.CobolTree.EnvironmentDivision, query));

            if (_cobolFile.CobolTree.DataDivision != null && _cobolFile.CobolTree.DataDivision.Length > 0)
                result.Nodes.Add(ConvertToTreeNode(_cobolFile.CobolTree.DataDivision, query));

            if (_cobolFile.CobolTree.ProcedureDivision != null && _cobolFile.CobolTree.ProcedureDivision.Length > 0)
                result.Nodes.Add(ConvertToTreeNode(_cobolFile.CobolTree.ProcedureDivision, query));

            if (result.Nodes.Count == 0 && _cobolFile.Variables.Count > 0)
                result.Nodes.AddRange(_cobolFile.GetLocalRootVariables()
                        .Select(variable => new TreeNode(variable.VariableName) { Tag = variable })
                        .OrderBy(node => ((Variable)node.Tag).Index)
                        .ToArray());

            return result;
        }

        private TreeNode ConvertToTreeNode(CobolTreeNode cobolTreeNode, string query = "")
        {
            var result = new TreeNode(cobolTreeNode.Name);
            foreach (var treeNode in cobolTreeNode.GetNodes())
            {
                var node = ConvertToTreeNode(treeNode, query);
                // if query is empty, match or Nodes contains match
                if (string.IsNullOrWhiteSpace(query) || node.Text.ContainsIgnoreCase(query) || node.Nodes.Count > 0)
                    result.Nodes.Add(node);
            }

            return result;
        }

        #endregion

        #region Sort by Perform

        private TreeNode GetPerformTree()
        {
            if (_cobolFile == null)
                return null;

            Logger.Info("Creating perform tree for file {0}", _cobolFile.Name);

            if (_cobolFile.CobolTree == null || _cobolFile.CobolTree.ProcedureDivision == null)
                return new TreeNode();

            var topNode = new TreeNode(_cobolFile.Name);

            var sections = _cobolFile.CobolTree.ProcedureDivision.Sections;

            // for all top-level-sections
            foreach (var section in sections.Where(sec => sec.IsReferencedBy.None()))
            {
                var node = new TreeNode(section.Name);

                foreach (var procedure in section.Procedures.Where(proc => proc.IsReferencedBy.None()))
                {
                    FindPerformsRecursively(node, procedure);
                }

                topNode.Nodes.Add(node);
            }

            if (topNode.Nodes.Count == 0 && _cobolFile.Variables.Count > 0)
                topNode.Nodes.AddRange(_cobolFile.GetLocalRootVariables()
                        .Select(variable => new TreeNode(variable.VariableName) { Tag = variable })
                        .OrderBy(node => ((Variable)node.Tag).Index)
                        .ToArray());

            return topNode;
        }

        private void FindPerformsRecursively(TreeNode topNode, Procedure procedure, int depth = 8)
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

            if (procedure is Section)
            {
                foreach (var proc in ((Section)procedure).Procedures)
                {
                    var newNode = new TreeNode(proc.Name);
                    topNode.Nodes.Add(newNode);
                    FindPerformsRecursively(newNode, proc, depth - 1);
                }
            }

            // create nodes for perform references and continue recusively
            foreach (var performReference in procedure.PerformReferences)
            {
                var newNode = new TreeNode(performReference.ReferencedProcedure);
                topNode.Nodes.Add(newNode);
                FindPerformsRecursively(newNode, performReference.Procedure, depth - 1);
            }
        }

        private TreeNode FilterByQuery(TreeNode treeNode, string query)
        {
            if (treeNode == null)
                return null;

            if (treeNode.Text.ContainsIgnoreCase(query))
                return treeNode;

            var childrenContainingQuery = treeNode.Nodes.Cast<TreeNode>()
                .Select(child => FilterByQuery(child, query))
                .Where(node => node != null).ToList();

            if (childrenContainingQuery.Any())
                return new TreeNode(treeNode.Text, childrenContainingQuery.ToArray());

            return null;
        }

        #endregion
    }
}
