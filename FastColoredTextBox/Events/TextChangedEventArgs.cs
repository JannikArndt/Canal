using System;

namespace FastColoredTextBoxNS.Events
{
    /// <summary>
    /// TextChanged event argument
    /// </summary>
    public class TextChangedEventArgs : EventArgs
    {
        /// <summary>
        /// Constructor
        /// </summary>
        public TextChangedEventArgs(Range changedRange)
        {
            ChangedRange = changedRange;
        }

        /// <summary>
        /// This range contains changed area of text
        /// </summary>
        public Range ChangedRange { get; set; }
    }
}