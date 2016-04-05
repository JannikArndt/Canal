using System;

namespace Logging
{
    public class LoggerEventArgs : EventArgs
    {
        public string Message { get; private set; }
        public int Priority { get; private set; }
        public LoggerEventArgs(int p, string m)
        {
            Priority = p;
            Message = m;
        }
    }
}