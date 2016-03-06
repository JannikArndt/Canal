using System.Windows.Forms;

namespace Canal.CobolTree.Models
{
    using System.Collections.Generic;
    using System.Linq;
    using System.Text.RegularExpressions;

    public class Procedure : TreeNode
    {
        public Procedure(string name, string text)
            : this(name)
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

        protected Procedure(string name) : base(name)
        {
            Name = name;
            PerformReferences = new List<PerformReference>();
            IsReferencedBy = new List<PerformReference>();
            CallReferences = new List<FileReference>();
            CopyReferences = new List<FileReference>();
        }

        public new string Name { get; set; }

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
