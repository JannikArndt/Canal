namespace Canal.CobolTree
{
    using System.Collections.Generic;
    using System.Linq;
    using System.Text.RegularExpressions;

    using Canal.Utils;

    public class Procedure : CobolTreeNode
    {
        public Procedure(string name, string text, int indexInSourceCode)
            : this(name, indexInSourceCode)
        {
            this.OriginalSource = text;

            var performReferenceMatches = Regex.Matches(text, @"PERFORM ([\w\d-]+) ?(THRU|UNTIL|WITH)? ?[\w\d-]* ?(UNTIL|BEFORE|AFTER)? ?[\w\d-<>=]*", RegexOptions.Compiled | RegexOptions.Multiline);

            foreach (Match performMatch in performReferenceMatches)
            {
                string procedureName = performMatch.Groups[1].ToString().Trim();
                if (this.PerformReferences.All(re => re.ReferenceName != procedureName))
                    this.PerformReferences.Add(new PerformReference(procedureName));
            }
        }

        protected Procedure(string name, int indexInSourceCode)
            : base(name, indexInSourceCode)
        {
            this.Name = name;
            this.PerformReferences = new List<PerformReference>();
            this.IsReferencedBy = new List<PerformReference>();
            this.CallReferences = new List<ProgramReference>();
            this.CopyReferences = new List<ProgramReference>();
            this.Variables = new List<Variable>();
        }

        public new string Name { get; set; }

        public string OriginalSource { get; set; }

        public List<PerformReference> PerformReferences { get; set; }

        public List<PerformReference> IsReferencedBy { get; set; }

        public List<ProgramReference> CallReferences { get; set; }

        public List<ProgramReference> CopyReferences { get; set; }

        public List<Variable> Variables { get; set; }

        public void AnalyzeVariables(List<Variable> variablesInFile)
        {
            var foundTokens = VariablesUtil.GetAllVariables(OriginalSource);

            foreach (var token in foundTokens)
            {
                var variable = variablesInFile.FindVariable(token);

                if (variable != null)
                    Variables.Add(variable);
            }
        }

        public override string ToString()
        {
            return this.Name;
        }
    }
}
