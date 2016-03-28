using System;
using System.Windows.Forms;

namespace FastColoredTextBoxNS.Events
{
    public class FunctionKeyPressedEventArgs : EventArgs
    {
        public KeyEventArgs Key { get; set; }

        public FunctionKeyPressedEventArgs(KeyEventArgs key)
        {
            Key = key;
        }
    }
}