﻿using System.Collections.Generic;

namespace Canal.CobolTree.Models
{
    using System.Text.RegularExpressions;

    public class Section : Procedure
    {
        public List<Procedure> Procedures { get; set; }

        public Section(string name, string sourceCode)
            : base(name)
        {
            Procedures = new List<Procedure>();

            var procedureNames = Regex.Matches(sourceCode, @"^ [\w\d-]+\.", RegexOptions.Compiled | RegexOptions.Multiline);

            foreach (Match procedureName in procedureNames)
            {
                string procName = procedureName.Value.Trim().Trim('.');
                var begin = procedureName.Index + procedureName.Length;
                var length = (procedureName.NextMatch().Success ? procedureName.NextMatch().Index : sourceCode.Length) - begin;
                string text = sourceCode.Substring(begin, length);
                Procedures.Add(new Procedure(procName, text));
            }

            foreach (var procedure in Procedures)
            {
                Nodes.Add(procedure);
            }
        }
    }
}
