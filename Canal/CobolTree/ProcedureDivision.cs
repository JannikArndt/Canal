namespace Canal.CobolTree
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text.RegularExpressions;

    public class ProcedureDivision : CobolTreeNode
    {
        public string OriginalSource { get; set; }

        public List<Section> Sections { get; set; }

        public ProcedureDivision(string sourceCode, int indexProcedureDivision)
            : base("Procedure Division", indexProcedureDivision)
        {
            this.OriginalSource = sourceCode;

            this.Sections = new List<Section>();

            var sectionNames = Regex.Matches(sourceCode, @"^ [\w\d-]+ SECTION\.", RegexOptions.Compiled | RegexOptions.Multiline);

            foreach (Match sectionName in sectionNames)
            {
                string name = sectionName.Value.Trim().Trim('.');
                var begin = sectionName.Index + sectionName.Length;
                var length = (sectionName.NextMatch().Success ? sectionName.NextMatch().Index : sourceCode.Length) - begin;
                string text = sourceCode.Substring(begin, length);
                this.Sections.Add(new Section(name, text, indexProcedureDivision + begin));
            }

            // Perform-Referenzen aufbauen
            var allProcedures = this.Sections.SelectMany(sec => sec.Procedures).ToList();
            var allProceduresAndSections = allProcedures.Union(this.Sections).ToList();

            foreach (var procedure in allProcedures)
            {
                foreach (var reference in procedure.PerformReferences)
                {
                    var referencedProcedure = allProceduresAndSections.FirstOrDefault(proc => proc.Name == reference.ReferenceName);

                    if (referencedProcedure != null)
                    {
                        reference.Procedure = referencedProcedure;
                        reference.Procedure.IsReferencedBy.Add(reference);
                    }
                    else
                    {
                        Console.WriteLine(@"Referenz nicht gefunden: " + reference.ReferenceName);
                    }
                }
            }

            foreach (var section in this.Sections)
            {
                this.Nodes.Add(section);
            }
        }
    }
}
