using Logging;
using Model;
using Model.References;
using System.Collections.Generic;
using System.ComponentModel;

namespace Canal.Utils
{
    using System;
    using System.Linq;
    using System.Text.RegularExpressions;
    using System.Windows.Forms;

    public class ReferenceUtil
    {
        public event ProgressChangedEventHandler ProgressChanged;

        public void ResolveCopys(CobolFile file)
        {
            Logger.Singleton.AddMsg(3, "Start resolving copys for file {0}", file.Name);

            var copyReferences = FindCopyReferences(file.Text).ToList();

            var counter = 0;
            foreach (var copyReference in copyReferences)
            {
                counter++;
                Logger.Singleton.AddMsg(3, "Resolving program {0} in folder {1}", copyReference.ProgramName, copyReference.Directory);

                var copyFile = FileUtil.Get(copyReference);

                if (copyFile == null)
                    continue;

                var updatedIndexOfCopy = file.Text.IndexOf(copyReference.ProgramName, StringComparison.Ordinal);
                var lineAfterCopy = file.Text.IndexOf(Environment.NewLine, updatedIndexOfCopy, StringComparison.Ordinal);
                file.Text = file.Text.Insert(lineAfterCopy + 1, copyFile.Text + Environment.NewLine);

                if (ProgressChanged != null)
                    ProgressChanged.Invoke(null, new ProgressChangedEventArgs(counter * 100 / (copyReferences.Count + 3), null));
            }

            var builder = new CobolTreeBuilder();
            builder.Build(file);

            if (ProgressChanged != null)
                ProgressChanged.Invoke(null, new ProgressChangedEventArgs(90, null));
        }

        public static IEnumerable<FileReference> FindCopyReferences(string text, bool textIsTrimmed = false)
        {
            Logger.Singleton.AddMsg(3, "Finding copy references...");

            var prefix = textIsTrimmed ? @"^" : @"^[\d]{6}";
            var copyRegex = new Regex(prefix + @" *COPY (?<program>[\w]+) +OF +(?<folder>[\w]+)\.", RegexOptions.Compiled | RegexOptions.IgnoreCase | RegexOptions.Multiline);

            var matches = copyRegex.Matches(text);
            Logger.Singleton.AddMsg(3, "Resolving {0} COPYs...", matches.Count);

            return from Match match in matches
                   select FileUtil.GetFileReference(match.Groups["program"].Value, match.Groups["folder"].Value);
        }

        public static TreeNode GetPerformTree(CobolFile file)
        {
            if (file == null)
                return null;

            Logger.Singleton.AddMsg(3, "Creating perform tree for file {0}", file.Name);

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

        private static void FindPerformsRecursively(TreeNode topNode, Procedure procedure)
        {
            Logger.Singleton.AddMsg(3, "Finding performs recursively for node {0}", topNode.Text);

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
