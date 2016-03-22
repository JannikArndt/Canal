using System;

namespace Canal.CobolTree
{
    using System.Collections.Generic;
    using System.Linq;
    using System.Text.RegularExpressions;

    using Utils;

    public class Procedure : CobolTreeNode
    {
        public new string Name { get; set; }

        public string OriginalSource { get; set; }

        #region References

        public List<PerformReference> PerformReferences { get; set; }

        public List<GoToReference> GoToReferences { get; set; }

        public List<ProcedureReference> References { get { return PerformReferences.Concat<ProcedureReference>(GoToReferences).ToList(); } }

        public List<PerformReference> IsReferencedBy { get; set; }

        public List<FileReference> CallReferences { get; set; }

        #endregion

        public Dictionary<Variable, UsedAs> Variables { get; set; }

        private readonly List<string> _lines;

        public int LinesOfCode { get { return _lines.Count; } }

        public Procedure(string name, string text, int indexInSourceCode)
            : this(name, indexInSourceCode)
        {
            OriginalSource = text;

            _lines = text.Split(Environment.NewLine.ToCharArray(), StringSplitOptions.RemoveEmptyEntries).ToList();
        }

        protected Procedure(string name, int indexInSourceCode)
            : base(name, indexInSourceCode)
        {
            Name = name;
            PerformReferences = new List<PerformReference>();
            GoToReferences = new List<GoToReference>();
            IsReferencedBy = new List<PerformReference>();
            CallReferences = new List<FileReference>();
            Variables = new Dictionary<Variable, UsedAs>();
        }

        public void AnalyzeVariables(List<Variable> variablesInFile)
        {
            var foundTokens = VariablesUtil.GetIdentifierLiterals(OriginalSource);

            foreach (Literal token in foundTokens)
            {
                Variable variable = variablesInFile.FindVariable(token.Name);

                if (variable != null)
                    if (Variables.ContainsKey(variable))
                        Variables[variable] = Variables[variable].MergeUsages(token);
                    else
                        Variables.Add(variable, token.UsedAs);
            }
        }

        public void AnalyzePerformReferences()
        {
            var performReferenceMatches = Regex.Matches(OriginalSource, Constants.Perform, RegexOptions.Compiled | RegexOptions.Multiline);

            foreach (Match performMatch in performReferenceMatches)
            {
                string procedureName = performMatch.Groups[1].ToString().Trim();
                if (PerformReferences.All(re => re.ReferencedProcedure != procedureName))
                    PerformReferences.Add(new PerformReference(procedureName));
            }
        }

        public void AnalyzeGoTos()
        {
            var gotoReferenceMatches = Regex.Matches(OriginalSource, Constants.GoTo, RegexOptions.Compiled | RegexOptions.Multiline);

            foreach (Match goToMatch in gotoReferenceMatches)
            {
                string procedureName = goToMatch.Groups[1].ToString().Trim();
                if (GoToReferences.All(re => re.ReferencedProcedure != procedureName))
                    GoToReferences.Add(new GoToReference(procedureName));
            }
        }


        public override string ToString()
        {
            return Name;
        }

        public void AnalyzeCalls()
        {
            var referenceMatches = Regex.Matches(OriginalSource, Constants.Call, RegexOptions.Compiled | RegexOptions.Multiline);

            foreach (Match match in referenceMatches)
            {
                string programName = match.Groups["literal"].ToString().Trim();

                var fileRefs = FileUtil.GetFileReferences(programName);

                if (fileRefs.Count > 1)
                    Console.WriteLine(@"error: ambiguous name");

                var fileRef = fileRefs.FirstOrDefault();

                if (fileRef == null) continue;

                fileRef.ReferencedIn.Add(this);

                if (!CallReferences.Contains(fileRef))
                    CallReferences.Add(fileRef);
            }
        }
    }
}
