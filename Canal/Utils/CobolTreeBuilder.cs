namespace Canal.Utils
{
    using System;
    using System.Collections.Generic;
    using System.Globalization;
    using System.Linq;
    using System.Text.RegularExpressions;

    using Logging;

    using Model;
    using Model.References;

    public class CobolTreeBuilder
    {
        public void Build(CobolFile file)
        {
            var tree = new CobolTree(file.Name);

            var sourceCode = TextUtil.TrimAllLines(file.Text);

            var indexIdentificationDivision = sourceCode.IndexOf("IDENTIFICATION DIVISION", StringComparison.Ordinal);
            var indexEnvironmentDivision = sourceCode.IndexOf("ENVIRONMENT DIVISION", StringComparison.Ordinal);
            var indexDataDivision = sourceCode.IndexOf("DATA DIVISION", StringComparison.Ordinal);
            var indexProcedureDivision = sourceCode.IndexOf("PROCEDURE DIVISION", StringComparison.Ordinal);

            if (indexIdentificationDivision < 0)
                Logger.Warning("Identification division not found.");
            if (indexEnvironmentDivision < 0)
                Logger.Warning("Environment division not found.");
            if (indexDataDivision < 0)
                Logger.Warning("Data division not found.");
            if (indexProcedureDivision < 0)
                Logger.Warning("Procedure division not found.");

            tree.IdentificationDivision = indexIdentificationDivision > 0
                ? new IdentificationDivision(sourceCode.Substring(indexIdentificationDivision, Math.Max(0, indexEnvironmentDivision - indexIdentificationDivision)), indexIdentificationDivision)
                : new IdentificationDivision("", 0);

            tree.EnvironmentDivision = indexEnvironmentDivision > 0
                ? new EnvironmentDivision(sourceCode.Substring(indexEnvironmentDivision, Math.Max(0, indexDataDivision - indexEnvironmentDivision)), indexEnvironmentDivision)
                : new EnvironmentDivision("", 0);

            tree.DataDivision = indexDataDivision > 0
                ? new DataDivision(sourceCode.Substring(indexDataDivision, Math.Max(0, indexProcedureDivision - indexDataDivision)), indexDataDivision)
                : new DataDivision("", 0);

            // if there are no divisions, everything goes into the data division
            if (indexIdentificationDivision < 0 && indexEnvironmentDivision < 0 && indexDataDivision < 0 &&
                indexProcedureDivision < 0)
            {
                Logger.Info("No divisions found => everything goes into the data division, hoping this is a record.");
                tree.DataDivision = new DataDivision(sourceCode, 0);
            }

            BuildDataDivision(tree.DataDivision);

            tree.ProcedureDivision = indexProcedureDivision > 0
                ? new ProcedureDivision(sourceCode.Substring(indexProcedureDivision, Math.Max(0, sourceCode.Length - indexProcedureDivision)), indexProcedureDivision)
                : new ProcedureDivision("", 0);

            BuildSections(tree.ProcedureDivision);
            BuildReferences(tree.ProcedureDivision);

            // TODO extract from Procedure
            foreach (var procedure in tree.AllProcedures)
            {
                AnalyzeVariables(procedure, tree.DataDivision.Variables);
                AnalyzePerformReferences(procedure);
                AnalyzeGoTos(procedure);
                AnalyzeCalls(procedure);
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
                {"Lines of Code", file.CobolTree.LinesOfCode.ToString(CultureInfo.InvariantCulture) }
            };
        }

        private void BuildDataDivision(DataDivision dataDivision)
        {
            int indexWorkingStorageSection = Math.Max(0, dataDivision.OriginalSource.IndexOf("WORKING-STORAGE SECTION", StringComparison.Ordinal));
            int indexLinkageSection = Math.Max(0, dataDivision.OriginalSource.IndexOf("LINKAGE SECTION", StringComparison.Ordinal));

            dataDivision.WorkingStorageSection = new WorkingStorageSection(
                dataDivision.OriginalSource.Substring(indexWorkingStorageSection, Math.Max(0, indexLinkageSection - indexWorkingStorageSection)),
                dataDivision.IndexInSource + indexWorkingStorageSection);

            BuildWorkingStorageSection(dataDivision.WorkingStorageSection);

            dataDivision.LinkageSection = new LinkageSection(
                dataDivision.OriginalSource.Substring(indexLinkageSection, dataDivision.OriginalSource.Length - indexLinkageSection),
                dataDivision.IndexInSource + indexLinkageSection);

            BuildLinkageSection(dataDivision.LinkageSection);

            dataDivision.Nodes.Add(dataDivision.WorkingStorageSection);
            dataDivision.Nodes.Add(dataDivision.LinkageSection);
        }

        private void BuildWorkingStorageSection(WorkingStorageSection workingStorageSection)
        {
            workingStorageSection.Variables = VariablesUtil.AnalyzeVariables(workingStorageSection.OriginalSource);
            workingStorageSection.CopyReferences = ReferenceUtil.FindCopyReferences(workingStorageSection.OriginalSource, true).ToList();
        }

        private void BuildLinkageSection(LinkageSection linkageSection)
        {
            linkageSection.Variables = VariablesUtil.AnalyzeVariables(linkageSection.OriginalSource);
            linkageSection.CopyReferences = ReferenceUtil.FindCopyReferences(linkageSection.OriginalSource, true).ToList();
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

        #region procedure

        public void AnalyzeVariables(Procedure procedure, List<Variable> variablesInFile)
        {
            var foundTokens = VariablesUtil.GetIdentifierLiterals(procedure.OriginalSource);

            foreach (Literal token in foundTokens)
            {
                Variable variable = variablesInFile.FindVariable(token.Name);

                if (variable != null)
                    if (procedure.Variables.ContainsKey(variable))
                        procedure.Variables[variable] = procedure.Variables[variable].MergeUsages(token);
                    else
                        procedure.Variables.Add(variable, token.UsedAs);
            }
        }

        public void AnalyzePerformReferences(Procedure procedure)
        {
            var performReferenceMatches = Regex.Matches(procedure.OriginalSource, Constants.Perform, RegexOptions.Compiled | RegexOptions.Multiline);

            foreach (Match performMatch in performReferenceMatches)
            {
                string procedureName = performMatch.Groups[1].ToString().Trim();
                if (procedure.PerformReferences.All(re => re.ReferencedProcedure != procedureName))
                    procedure.PerformReferences.Add(new PerformReference(procedureName));
            }
        }

        public void AnalyzeGoTos(Procedure procedure)
        {
            var gotoReferenceMatches = Regex.Matches(procedure.OriginalSource, Constants.GoTo, RegexOptions.Compiled | RegexOptions.Multiline);

            foreach (Match goToMatch in gotoReferenceMatches)
            {
                string procedureName = goToMatch.Groups[1].ToString().Trim();
                if (procedure.GoToReferences.All(re => re.ReferencedProcedure != procedureName))
                    procedure.GoToReferences.Add(new GoToReference(procedureName));
            }
        }

        public void AnalyzeCalls(Procedure procedure)
        {
            var referenceMatches = Regex.Matches(procedure.OriginalSource, Constants.Call, RegexOptions.Compiled | RegexOptions.Multiline);

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
