using System;
using System.Collections.Generic;

namespace Canal
{
    public class CobolFile
    {
        private IEnumerable<string> lines;
        internal string Name;

        public string Text { get { return string.Join(Environment.NewLine, lines); } set { throw new NotImplementedException(); } }

        public CobolFile(IEnumerable<string> lines)
        {
            this.lines = lines;
        }
    }
}