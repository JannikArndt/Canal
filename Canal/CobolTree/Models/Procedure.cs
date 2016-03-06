namespace Canal.CobolTree.Models
{
    using System.Collections.Generic;
    using System.Linq;
    using System.Text.RegularExpressions;

    public class Procedure : CobolTreeNode
    {
        public Procedure(string name, string text, int indexInSourceCode)
            : this(name, indexInSourceCode)
        {
            OriginalSource = text;

            var performReferenceMatches = Regex.Matches(text, @"PERFORM ([\w\d-]+) ?(THRU|UNTIL|WITH)? ?[\w\d-]* ?(UNTIL|BEFORE|AFTER)? ?[\w\d-<>=]*", RegexOptions.Compiled | RegexOptions.Multiline);

            foreach (Match performMatch in performReferenceMatches)
            {
                string procedureName = performMatch.Groups[1].ToString().Trim();
                if (PerformReferences.All(re => re.ReferenceName != procedureName))
                    PerformReferences.Add(new PerformReference(procedureName));
            }
        }

        protected Procedure(string name, int indexInSourceCode) : base(name, indexInSourceCode)
        {
            Name = name;
            PerformReferences = new List<PerformReference>();
            IsReferencedBy = new List<PerformReference>();
            CallReferences = new List<ProgramReference>();
            CopyReferences = new List<ProgramReference>();
        }

        public new string Name { get; set; }

        public string OriginalSource { get; set; }

        public List<PerformReference> PerformReferences { get; set; }

        public List<PerformReference> IsReferencedBy { get; set; }

        public List<ProgramReference> CallReferences { get; set; }

        public List<ProgramReference> CopyReferences { get; set; }

        public override string ToString()
        {
            return Name;
        }
    }
}
