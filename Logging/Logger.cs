namespace Logging
{
    using System;
    using System.Collections.Generic;

    public class Logger
    {
        private readonly List<LoggerEventArgs> logList = new List<LoggerEventArgs>();

        static Logger()
        {
            Singleton = new Logger();
        }

        private Logger()
        {
        }

        public event EventHandler<LoggerEventArgs> Log;

        public static Logger Singleton { get; private set; }

        public static void Info(string msg, params object[] arguments)
        {
            Singleton.AddMsg(LoggingLevel.Info, msg, arguments);
        }

        public static void Warning(string msg, params object[] arguments)
        {
            Singleton.AddMsg(LoggingLevel.Warning, msg, arguments);
        }

        public static void Error(string msg, params object[] arguments)
        {
            Singleton.AddMsg(LoggingLevel.Error, msg, arguments);
        }

        public List<LoggerEventArgs> GetEvents()
        {
            return this.logList;
        }

        private void AddMsg(LoggingLevel level, string msg)
        {
            var evt = new LoggerEventArgs(level, msg);
            this.logList.Add(evt);

            var l = Log; // Keep reference
            if (l != null)
                l(this, evt);
        }

        private void AddMsg(LoggingLevel level, string msg, params object[] arguments)
        {
            AddMsg(level, string.Format(msg, arguments));
        }
    }
}
