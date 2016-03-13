using System;
using System.Collections.Generic;
using System.Linq;

namespace Canal.CobolTree
{
    public abstract class Division : CobolTreeNode
    {
        public string OriginalSource { get; set; }

        protected readonly List<string> _lines;

        public int LinesOfCode { get { return _lines.Count; } }

        protected Division(string sourceCode, string nodeName, int indexDataDivision) : base(nodeName, indexDataDivision)
        {
            OriginalSource = sourceCode;

            _lines = sourceCode.Split(Environment.NewLine.ToCharArray(), StringSplitOptions.RemoveEmptyEntries).ToList();
        }
    }
}