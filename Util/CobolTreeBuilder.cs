using Model;
using Model.References;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace Util
{
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

            foreach (var procedure in tree.GetAllProcedures())
            {
                AnalyzeVariables(procedure, file);
                AnalyzePerformReferences(procedure);
                AnalyzeGoTos(procedure);
                AnalyzeCalls(procedure);
            }

            BuildReferences(tree.ProcedureDivision);

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

        #region Analysis

        private void AnalyzeVariables(Procedure procedure, CobolFile cobolFile)
        {
            var foundTokens = VariablesUtil.Instance.GetIdentifierLiterals(procedure.GetCode());

            foreach (Literal token in foundTokens)
            {
                Variable variable;
                if (cobolFile.Variables.TryGetValue(token.Name, out variable))
                    procedure.VariableUsages.AddOrUpdate(variable, token.UsedAs, (vari, usedAs) => usedAs.MergeUsages(token));
            }
        }

        private void AnalyzePerformReferences(Procedure procedure)
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

        private void AnalyzeGoTos(Procedure procedure)
        {
            var gotoReferenceMatches = Regex.Matches(procedure.GetCode(), Constants.GoTo, RegexOptions.Compiled | RegexOptions.Multiline);

            foreach (Match goToMatch in gotoReferenceMatches)
            {
                string procedureName = goToMatch.Groups[1].ToString().Trim();
                if (procedure.GoToReferences.All(re => re.ReferencedProcedure != procedureName))
                    procedure.GoToReferences.Add(new GoToReference(procedureName));
            }
        }

        private void AnalyzeCalls(Procedure procedure)
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
    }
}
