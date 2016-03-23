using System;
using System.Collections.Generic;

namespace FastColoredTextBoxNS.Events
{
    public class WordWrapNeededEventArgs : EventArgs
    {
        public List<int> CutOffPositions { get; private set; }
        public bool ImeAllowed { get; private set; }
        public Line Line { get; private set; }

        public WordWrapNeededEventArgs(List<int> cutOffPositions, bool imeAllowed, Line line)
        {
            this.CutOffPositions = cutOffPositions;
            this.ImeAllowed = imeAllowed;
            this.Line = line;
        }
    }
}