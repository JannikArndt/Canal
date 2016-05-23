using System.Collections.Generic;
using System.Windows.Forms;

namespace Canal.Utils
{
    using Model;
    using Model.References;
    using System;
    using System.Linq;
    using System.Text.RegularExpressions;

    public class CobolTreeBuilder
    {
        public CobolTree Build(CobolFile file)
        {
            var tree = new CobolTree();

            if (!file.DivisionsAndSection.AllDivisionsFound())
                return tree;

            // IDENTIFICATION DIVISION
            tree.IdentificationDivision = new IdentificationDivision(file);

            // ENVIRONMENT DIVISION
            tree.EnvironmentDivision = new EnvironmentDivision(file);

            // DATA DIVISION
            tree.DataDivision = new DataDivision(file)
            {
                WorkingStorageSection = new WorkingStorageSection(file),
                LinkageSection = new LinkageSection(file)
            };

            // PROCEDURE DIVISION
            tree.ProcedureDivision = new ProcedureDivision(file);
            tree.ProcedureDivision.Sections = BuildSections(tree.ProcedureDivision);
            BuildReferences(tree.ProcedureDivision);

            foreach (var procedure in tree.GetAllProcedures())
            {
                AnalyzeVariables(procedure, file);
                AnalyzePerformReferences(procedure);
                AnalyzeGoTos(procedure);
                AnalyzeCalls(procedure);
            }

            // fix deeper references
            foreach (var performReference in tree.GetAllProcedures().SelectMany(procedure => procedure.PerformReferences.Where(pref => pref.Procedure == null)))
            {
                performReference.Procedure = tree.GetAllProcedures().FirstOrDefault(p => p.Name == performReference.ReferencedProcedure);
            }

            return tree;
        }

        private List<Section> BuildSections(ProcedureDivision procedureDivision)
        {
            var result = new List<Section>();
            var code = procedureDivision.GetCode();

            foreach (Match sectionName in Constants.SectionRegex.Matches(code))
            {
                var name = sectionName.Groups["sectionName"].Value;
                var begin = procedureDivision.StartIndex + sectionName.Index;
                var end = procedureDivision.StartIndex + (sectionName.NextMatch().Success ? sectionName.NextMatch().Index : procedureDivision.Length);

                var section = new Section(procedureDivision.ParentCobolFile, name, begin, end);
                section.Procedures = BuildProcedures(section);

                result.Add(section);
            }

            return result;
        }

        private List<Procedure> BuildProcedures(Section section)
        {
            var result = new List<Procedure>();
            var code = section.GetCode();

            foreach (Match procedureName in Constants.ProcedureRegex.Matches(code))
            {
                var procName = procedureName.Groups["procedureName"].Value;
                var begin = section.StartIndex + procedureName.Index;
                var end = section.StartIndex + (procedureName.NextMatch().Success ? procedureName.NextMatch().Index : section.Length);

                result.Add(new Procedure(section.ParentCobolFile, procName, begin, end));
            }

            return result;
        }

        private void BuildReferences(ProcedureDivision procedureDivision)
        {
            // Perform-Referenzen aufbauen
            var allProcedures = procedureDivision.Sections.SelectMany(sec => sec.Procedures).ToList();
            var allProceduresAndSections = allProcedures.Union(procedureDivision.Sections).ToList();

            foreach (var procedure in allProcedures)
            {
                foreach (var reference in procedure.PerformReferences)
                {
                    var referencedProcedure = allProceduresAndSections.FirstOrDefault(proc => proc.Name == reference.ReferencedProcedure);

                    if (referencedProcedure != null)
                    {
                        reference.Procedure = referencedProcedure;
                        reference.Procedure.IsReferencedBy.Add(reference);
                    }
                    else
                    {
                        Console.WriteLine(@"Referenz nicht gefunden: " + reference.ReferencedProcedure);
                    }
                }
            }
        }

        #region procedure

        public void AnalyzeVariables(Procedure procedure, CobolFile cobolFile)
        {
            var foundTokens = VariablesUtil.Instance.GetIdentifierLiterals(procedure.GetCode());

            foreach (Literal token in foundTokens)
            {
                Variable variable;
                if (cobolFile.Variables.TryGetValue(token.Name, out variable))
                    procedure.Variables.AddOrUpdate(variable, token.UsedAs, (vari, usedAs) => usedAs.MergeUsages(token));
            }
        }

        public void AnalyzePerformReferences(Procedure procedure)
        {
            var code = procedure.GetCode();
            var performReferenceMatches = Regex.Matches(code, Constants.Perform, RegexOptions.Compiled | RegexOptions.Multiline);

            foreach (Match performMatch in performReferenceMatches)
            {
                string procedureName = performMatch.Groups[1].ToString().Trim();
                if (procedure.PerformReferences.All(re => re.ReferencedProcedure != procedureName))
                    procedure.PerformReferences.Add(new PerformReference(procedureName));
            }
        }

        public void AnalyzeGoTos(Procedure procedure)
        {
            var gotoReferenceMatches = Regex.Matches(procedure.GetCode(), Constants.GoTo, RegexOptions.Compiled | RegexOptions.Multiline);

            foreach (Match goToMatch in gotoReferenceMatches)
            {
                string procedureName = goToMatch.Groups[1].ToString().Trim();
                if (procedure.GoToReferences.All(re => re.ReferencedProcedure != procedureName))
                    procedure.GoToReferences.Add(new GoToReference(procedureName));
            }
        }

        public void AnalyzeCalls(Procedure procedure)
        {
            var referenceMatches = Regex.Matches(procedure.GetCode(), Constants.Call, RegexOptions.Compiled | RegexOptions.Multiline);

            foreach (Match match in referenceMatches)
            {
                string programName = match.Groups["literal"].ToString().Trim();

                var fileRefs = FileUtil.Instance.GetFileReferences(programName);

                if (fileRefs.Count > 1)
                    Console.WriteLine(@"error: ambiguous name");

                var fileRef = fileRefs.FirstOrDefault();

                if (fileRef == null) continue;

                fileRef.ReferencedIn.Add(procedure);

                if (!procedure.CallReferences.Contains(fileRef))
                    procedure.CallReferences.Add(fileRef);
            }
        }

        #endregion

        public static TreeNode ConvertToTreeNodes(CobolTree cobolTree, string name, string query = "")
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

        public static TreeNode ConvertToTreeNode(CobolTreeNode cobolTreeNode, string query = "")
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

        public static TreeNode ConvertToFlatToc(CobolTree cobolTree, string name, string query = "")
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
    }
}
