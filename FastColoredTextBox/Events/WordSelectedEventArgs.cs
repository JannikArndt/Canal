using System;

namespace FastColoredTextBoxNS.Events
{
    public class WordSelectedEventArgs : EventArgs
    {
        public string Word { get; set; }
        public WordSelectedEventArgs(string word)
        {
            Word = word;
        }
    }
}