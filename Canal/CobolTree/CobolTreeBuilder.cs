using Canal.Utils;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading;

namespace Canal.CobolTree
{
    public class CobolTreeBuilder
    {
        public void Build(CobolFile file)
        {
            Thread.Sleep(3000);

            var tree = new CobolTree(file.Name);

            var sourceCode = TextUtil.TrimAllLines(file.Text);

            var indexIdentificationDivision = sourceCode.IndexOf("IDENTIFICATION DIVISION", StringComparison.Ordinal);
            var indexEnvironmentDivision = sourceCode.IndexOf("ENVIRONMENT DIVISION", StringComparison.Ordinal);
            var indexDataDivision = sourceCode.IndexOf("DATA DIVISION", StringComparison.Ordinal);
            var indexProcedureDivision = sourceCode.IndexOf("PROCEDURE DIVISION", StringComparison.Ordinal);

            tree.IdentificationDivision = indexIdentificationDivision > 0
                ? new IdentificationDivision(sourceCode.Substring(indexProcedureDivision, Math.Max(0, indexEnvironmentDivision - indexIdentificationDivision)), indexIdentificationDivision)
                : new IdentificationDivision("", 0);

            tree.EnvironmentDivision = indexEnvironmentDivision > 0
                ? new EnvironmentDivision(sourceCode.Substring(indexEnvironmentDivision, Math.Max(0, indexDataDivision - indexEnvironmentDivision)), indexEnvironmentDivision)
                : new EnvironmentDivision("", 0);

            tree.DataDivision = indexDataDivision > 0
                ? new DataDivision(sourceCode.Substring(indexDataDivision, Math.Max(0, indexProcedureDivision - indexDataDivision)), indexDataDivision)
                : new DataDivision("", 0);

            tree.ProcedureDivision = indexProcedureDivision > 0
                ? new ProcedureDivision(sourceCode.Substring(indexProcedureDivision, Math.Max(0, sourceCode.Length - indexProcedureDivision)), indexProcedureDivision)
                : new ProcedureDivision("", 0);

            BuildSections(tree.ProcedureDivision);
            BuildReferences(tree.ProcedureDivision);

            // TODO extract from Procedure
            foreach (var procedure in tree.AllProcedures)
            {
                procedure.AnalyzeVariables(tree.DataDivision.Variables);
                procedure.AnalyzePerformReferences();
                procedure.AnalyzeGoTos();
                procedure.AnalyzeCalls();
            }

            // fix deeper references
            foreach (var performReference in tree.AllProcedures.SelectMany(procedure => procedure.PerformReferences.Where(pref => pref.Procedure == null)))
            {
                performReference.Procedure = tree.AllProcedures.FirstOrDefault(p => p.Name == performReference.ReferencedProcedure);
            }

            file.CobolTree = tree;

            // Add infos
            file.Infos = new Dictionary<string, string>
            {
                {"Name", file.Name },
                {"Lines of Code", file.CobolTree.LinesOfCode.ToString() }
            };
        }

        private void BuildSections(ProcedureDivision procedureDivision)
        {
            var sectionNames = Regex.Matches(procedureDivision.OriginalSource, @"^ [\w\d-]+ SECTION\.", RegexOptions.Compiled | RegexOptions.Multiline);

            foreach (Match sectionName in sectionNames)
            {
                var name = sectionName.Value.Trim().Trim('.');
                var begin = sectionName.Index + sectionName.Length;
                var length = (sectionName.NextMatch().Success ? sectionName.NextMatch().Index : procedureDivision.OriginalSource.Length) - begin;
                var text = procedureDivision.OriginalSource.Substring(begin, length);

                var section = BuildSection(name, procedureDivision.IndexInSource + begin, text);

                procedureDivision.Sections.Add(section);
            }
        }

        private Section BuildSection(string name, int indexInSourceCode, string sourceCode)
        {
            var section = new Section(name, indexInSourceCode);

            var procedureNames = Regex.Matches(sourceCode, @"^ [\w\d-]+\.", RegexOptions.Compiled | RegexOptions.Multiline);

            foreach (Match procedureName in procedureNames)
            {
                string procName = procedureName.Value.Trim().Trim('.');
                var begin = procedureName.Index + procedureName.Length;
                var length = (procedureName.NextMatch().Success ? procedureName.NextMatch().Index : sourceCode.Length) - begin;
                string text = sourceCode.Substring(begin, length);
                section.Procedures.Add(new Procedure(procName, text, indexInSourceCode + begin));
            }

            foreach (var procedure in section.Procedures)
            {
                section.Nodes.Add(procedure);
            }

            return section;
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

            foreach (var section in procedureDivision.Sections)
            {
                procedureDivision.Nodes.Add(section);
            }
        }
    }
}
