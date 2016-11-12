using System;

namespace FastColoredTextBoxNS.Events
{
    public class WordSelectedEventArgs : EventArgs
    {
        public string Word { get; private set; }

        public string LookFor { get; private set; }

        public WordSelectedEventArgs(string word, string lookFor = "")
        {
            Word = word;
            LookFor = lookFor;
        }
    }
}