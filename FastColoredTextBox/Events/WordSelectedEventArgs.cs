using System;

namespace FastColoredTextBoxNS.Events
{
    public class WordSelectedEventArgs : EventArgs
    {
        public string Word { get; }

        public string LookFor { get; }

        public WordSelectedEventArgs(string word, string lookFor = "")
        {
            Word = word;
            LookFor = lookFor;
        }
    }
}