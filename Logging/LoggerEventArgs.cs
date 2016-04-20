namespace Logging
{
    using System;

    public class LoggerEventArgs : EventArgs
    {
        public LoggingLevel Level { get; private set; }

        public string Message { get; private set; }

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