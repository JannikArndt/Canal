using System;

namespace Logging
{
    public class LoggerEventArgs : EventArgs
    {
        public LoggingLevel Level { get; }

        public string Message { get; }

        public LoggerEventArgs(LoggingLevel level, string message)
        {
            Level = level;
            Message = message;
        }

        public override string ToString()
        {
            return string.Format("{0}\t{1}", Level, Message);
        }
    }
}