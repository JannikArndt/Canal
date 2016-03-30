using System;
using System.Collections.Generic;
using System.Linq;

namespace Model
{
    public abstract class Division : CobolTreeNode
    {
        public string OriginalSource { get; private set; }

        protected readonly List<string> Lines;

        public int LinesOfCode { get { return Lines.Count; } }

        protected Division(string sourceCode, string nodeName, int indexDataDivision) : base(nodeName, indexDataDivision)
        {
            OriginalSource = sourceCode;

            Lines = sourceCode.Split(Environment.NewLine.ToCharArray(), StringSplitOptions.RemoveEmptyEntries).ToList();
        }
    }
}