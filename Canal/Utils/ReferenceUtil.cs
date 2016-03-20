namespace Canal.Utils
{
    using CobolTree;
    using System;
    using System.Linq;
    using System.Text.RegularExpressions;
    using System.Windows.Forms;

    public static class ReferenceUtil
    {
        public static void ResolveCopys(CobolFile file)
        {
            var copyRegex = new Regex(
                @"^[\d ]{7}COPY (?<program>[\w]+) +OF +(?<folder>[\w]+)\.",
                RegexOptions.Compiled | RegexOptions.IgnoreCase | RegexOptions.Multiline);

            var matches = copyRegex.Matches(file.Text);
            Console.WriteLine("Resolving " + matches.Count + " COPYs...");

            foreach (Match match in matches)
            {
                var programName = match.Groups["program"].Value;
                var folderName = match.Groups["folder"].Value;
                Console.WriteLine("Resolving program " + programName + " in folder " + folderName);

                var copyFile = FileUtil.Get(programName, folderName);

                if (copyFile == null)
                    continue;

                var updatedIndexOfCopy = file.Text.IndexOf(programName, match.Index - 1, StringComparison.Ordinal);
                var lineAfterCopy = file.Text.IndexOf(Environment.NewLine, updatedIndexOfCopy, StringComparison.Ordinal);
                file.Text = file.Text.Insert(lineAfterCopy + 1, copyFile.Text + Environment.NewLine);
            }

            file.RebuildTree();
        }

        public static TreeNode GetPerformTree(CobolFile file)
        {
            if (file == null || file.CobolTree == null || file.CobolTree.ProcedureDivision == null)
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

        private static void FindPerformsRecursively(TreeNode topNode, Procedure procedure)
        {
            if (procedure == null)
                return;

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
                FindPerformsRecursively(newNode, performReference.Procedure);
            }
        }
    }
}
