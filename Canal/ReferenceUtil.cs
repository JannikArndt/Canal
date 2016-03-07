using System;
using System.Text.RegularExpressions;

namespace Canal
{
    using System.Linq;
    using System.Windows.Forms;

    using Canal.CobolTree.Models;

    public static class ReferenceUtil
    {
        public static void ResolveCopys(CobolFile file)
        {
            var copyRegex = new Regex(@"^[\d ]{7}COPY (?<program>[\w]+)( OF (?<folder>[\w]+)\.)", RegexOptions.Compiled | RegexOptions.IgnoreCase | RegexOptions.Multiline);

            foreach (Match match in copyRegex.Matches(file.Text))
            {
                var copyFile = FileUtil.Get(match.Groups["program"].Value, match.Groups["folder"].Value);

                if (copyFile == null)
                    continue;

                var lineAfterCopy = file.Text.IndexOf(Environment.NewLine, match.Index, StringComparison.Ordinal);
                file.Text = file.Text.Insert(lineAfterCopy + 1, copyFile.Text + Environment.NewLine);
            }
        }

        public static TreeNode GetPerformTree(CobolFile file)
        {
            var topNode = new TreeNode(file.Name);

            var sections = file.CobolTree.ProcedureDivision.Sections;

            // for all top-level-sections
            foreach (var section in sections.Where(sec => !sec.IsReferencedBy.Any()))
            {
                // TODO Direkt FindPerformsRecursively ?
                var node = new TreeNode(section.Name);

                foreach (var procedure in section.Procedures)
                {
                    FindPerformsRecursively(node, procedure);
                }

                topNode.Nodes.Add(node);
            }

            return topNode;
        }

        private static void FindPerformsRecursively(TreeNode topNode, Procedure procedure)
        {
            if (procedure == null)
                return;

            foreach (var performReference in procedure.PerformReferences)
            {
                var newNode = new TreeNode(performReference.ReferenceName);
                FindPerformsRecursively(newNode, performReference.Procedure);
                topNode.Nodes.Add(newNode);
            }
        }
    }
}
