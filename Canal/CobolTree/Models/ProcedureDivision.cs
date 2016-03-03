namespace Canal.CobolTree.Models
{
    using System.Collections.Generic;
    using System.Text.RegularExpressions;

    public class ProcedureDivision
    {
        public string OriginalSource { get; set; }

        public List<Section> Sections { get; set; }

        public ProcedureDivision(string sourceCode)
        {
            this.OriginalSource = sourceCode;

            var procedureNames = Regex.Matches(sourceCode, @"^[\w\d-]+\.", RegexOptions.Compiled | RegexOptions.Multiline);

            foreach (Match procedureName in procedureNames)
            {
                string name = procedureName.Value;
                string text = sourceCode.Substring(procedureName.Index + procedureName.Length, procedureName.NextMatch().Index);
                this.Sections.Add(new Section(name, text));
            }
        }
    }
}
