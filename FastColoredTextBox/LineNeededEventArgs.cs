using System;

namespace FastColoredTextBoxNS
{
    public class LineNeededEventArgs : EventArgs
    {
        public string SourceLineText { get; private set; }
        public int DisplayedLineIndex { get; private set; }
        /// <summary>
        /// This text will be displayed in textbox
        /// </summary>
        public string DisplayedLineText { get; set; }

        public LineNeededEventArgs(string sourceLineText, int displayedLineIndex)
        {
            this.SourceLineText = sourceLineText;
            this.DisplayedLineIndex = displayedLineIndex;
            this.DisplayedLineText = sourceLineText;
        }
    }
}