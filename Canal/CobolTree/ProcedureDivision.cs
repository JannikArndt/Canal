namespace Canal.CobolTree
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text.RegularExpressions;

    public class ProcedureDivision : Division
    {
        public List<Section> Sections { get; set; }

        public new int LinesOfCode { get { return Sections.Sum(sec => sec.LinesOfCode) + 1; } }

        public ProcedureDivision(string sourceCode, int indexProcedureDivision)
            : base(sourceCode, "Procedure Division", indexProcedureDivision)
        {
            Sections = new List<Section>();

            var sectionNames = Regex.Matches(sourceCode, @"^ [\w\d-]+ SECTION\.", RegexOptions.Compiled | RegexOptions.Multiline);

            foreach (Match sectionName in sectionNames)
            {
                var name = sectionName.Value.Trim().Trim('.');
                var begin = sectionName.Index + sectionName.Length;
                var length = (sectionName.NextMatch().Success ? sectionName.NextMatch().Index : sourceCode.Length) - begin;
                var text = sourceCode.Substring(begin, length);
                Sections.Add(new Section(name, text, indexProcedureDivision + begin));
            }

            // Perform-Referenzen aufbauen
            var allProcedures = Sections.SelectMany(sec => sec.Procedures).ToList();
            var allProceduresAndSections = allProcedures.Union(Sections).ToList();

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

            foreach (var section in Sections)
            {
                Nodes.Add(section);
            }
        }
    }
}
