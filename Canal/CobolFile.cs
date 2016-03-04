using System;
using System.Collections.Generic;

namespace Canal
{
    public class CobolFile
    {
        private IEnumerable<string> lines;

        internal string Name;

        private CobolTree.Models.CobolTree cobolTree;

        public string Text { get { return string.Join(Environment.NewLine, lines); } set { throw new NotImplementedException(); } }

        public CobolTree.Models.CobolTree CobolTree
        {
            get
            {
                if (cobolTree == null)
                    cobolTree = new CobolTree.Models.CobolTree(Text);
                return this.cobolTree;
            }
            set
            {
                this.cobolTree = value;
            }
        }

        public CobolFile(IEnumerable<string> lines)
        {
            this.lines = lines;
        }
    }
}