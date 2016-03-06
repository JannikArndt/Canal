using System;
using System.Collections.Generic;
using System.IO;

namespace Canal
{
    public class CobolFile
    {
        private IEnumerable<string> lines;

        internal string Name;

        private CobolTree.Models.CobolTree _cobolTree;

        public string Text { get { return string.Join(Environment.NewLine, lines); } set { throw new NotImplementedException(); } }

        public CobolTree.Models.CobolTree CobolTree
        {
            get { return _cobolTree ?? (_cobolTree = new CobolTree.Models.CobolTree(Text, Name)); }
            set
            {
                _cobolTree = value;
            }
        }

        public CobolFile(IEnumerable<string> lines, string filename)
        {
            Name = Path.GetFileNameWithoutExtension(filename);
            this.lines = lines;
        }
    }
}