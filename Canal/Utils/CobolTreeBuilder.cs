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
            var tree = new CobolTree(file.Name);

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
            BuildSections(file, tree.ProcedureDivision);
            BuildReferences(tree.ProcedureDivision);

            foreach (var procedure in tree.AllProcedures)
            {
                AnalyzeVariables(procedure, file);
                AnalyzePerformReferences(procedure);
                AnalyzeGoTos(procedure);
                AnalyzeCalls(procedure);
            }

            // fix deeper references
            foreach (var performReference in tree.AllProcedures.SelectMany(procedure => procedure.PerformReferences.Where(pref => pref.Procedure == null)))
            {
                performReference.Procedure = tree.AllProcedures.FirstOrDefault(p => p.Name == performReference.ReferencedProcedure);
            }

            return tree;
        }

        private void BuildSections(CobolFile cobolFile, ProcedureDivision procedureDivision)
        {
            var code = procedureDivision.GetCode();

            foreach (Match sectionName in Constants.SectionRegex.Matches(code))
            {
                var name = sectionName.Groups["sectionName"].Value;
                var begin = sectionName.Index + sectionName.Length;
                var end = sectionName.NextMatch().Success ? sectionName.NextMatch().Index : procedureDivision.Length;

                var section = new Section(cobolFile, name, begin, end);
                BuildProcedures(cobolFile, section);

                procedureDivision.Sections.Add(section);
            }
        }


        private void BuildProcedures(CobolFile cobolFile, Section section)
        {
            var code = section.GetCode();

            foreach (Match procedureName in Constants.ProcedureRegex.Matches(code))
            {
                var procName = procedureName.Groups["procedureName"].Value;
                var begin = procedureName.Index + procedureName.Length;
                var end = procedureName.NextMatch().Success ? procedureName.NextMatch().Index : section.Length;
                section.Procedures.Add(new Procedure(cobolFile, procName, begin, end));
            }
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
            var performReferenceMatches = Regex.Matches(procedure.GetCode(), Constants.Perform, RegexOptions.Compiled | RegexOptions.Multiline);

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

        public static TreeNode ConvertToTreeNodes(CobolTree cobolTree, string name)
        {
            var result = new TreeNode(name);
            if (cobolTree.IdentificationDivision != null)
                result.Nodes.Add(ConvertToTreeNode(cobolTree.IdentificationDivision));

            if (cobolTree.EnvironmentDivision != null)
                result.Nodes.Add(ConvertToTreeNode(cobolTree.EnvironmentDivision));

            if (cobolTree.DataDivision != null)
                result.Nodes.Add(ConvertToTreeNode(cobolTree.DataDivision));

            if (cobolTree.ProcedureDivision != null)
                result.Nodes.Add(ConvertToTreeNode(cobolTree.ProcedureDivision));
            return result;
        }

        public static TreeNode ConvertToTreeNode(CobolTreeNode cobolTreeNode)
        {
            var result = new TreeNode(cobolTreeNode.Name);
            foreach (var treeNode in cobolTreeNode.GetNodes())
            {
                result.Nodes.Add(ConvertToTreeNode(treeNode));
            }

            return result;
        }

        public static TreeNode ConvertToFlatToc(CobolTree cobolTree, string name)
        {
            var result = new TreeNode(name);

            AddFlattenedOrderedIfNotNull(result, cobolTree.IdentificationDivision);
            AddFlattenedOrderedIfNotNull(result, cobolTree.EnvironmentDivision);
            AddFlattenedOrderedIfNotNull(result, cobolTree.DataDivision);
            AddFlattenedOrderedIfNotNull(result, cobolTree.ProcedureDivision);

            return result;
        }

        private static void AddFlattenedOrderedIfNotNull(TreeNode result, CobolTreeNode parent)
        {
            if (parent == null)
                return;

            var orderedFlatNode = new TreeNode(parent.Name, Flatten(parent, true).OrderBy(node => node.Text).ToArray());
            result.Nodes.Add(orderedFlatNode);
        }

        private static IEnumerable<TreeNode> Flatten(CobolTreeNode parent, bool ignoreFirst = false)
        {
            if (!ignoreFirst)
                yield return new TreeNode(parent.Name);

            foreach (var child in parent.GetNodes()) // check null if you must
                foreach (var relative in Flatten(child))
                    yield return new TreeNode(relative.Text);
        }
    }
}
