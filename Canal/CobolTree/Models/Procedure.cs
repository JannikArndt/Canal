namespace Canal.CobolTree.Models
{
    using System.Collections.Generic;
    using System.Linq;
    using System.Text.RegularExpressions;

    public class Procedure
    {
        public Procedure(string name, string text)
            : this(name)
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

        protected Procedure(string name)
        {
            this.Name = name;
            this.PerformReferences = new List<PerformReference>();
            this.IsReferencedBy = new List<PerformReference>();
            this.CallReferences = new List<FileReference>();
            this.CopyReferences = new List<FileReference>();
        }

        public string Name { get; set; }

        public string OriginalSource { get; set; }

        public List<PerformReference> PerformReferences { get; set; }

        public List<PerformReference> IsReferencedBy { get; set; }

        public List<FileReference> CallReferences { get; set; }

        public List<FileReference> CopyReferences { get; set; }

        public override string ToString()
        {
            return Name;
        }
    }
}
