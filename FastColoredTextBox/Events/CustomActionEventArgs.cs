using System;
using FastColoredTextBoxNS.Enums;

namespace FastColoredTextBoxNS.Events
{
    /// <summary>
    /// CustomAction event args
    /// </summary>
    public class CustomActionEventArgs : EventArgs
    {
        public FCTBAction Action { get; private set; }

        public CustomActionEventArgs(FCTBAction action)
        {
            Action = action;
        }
    }
}