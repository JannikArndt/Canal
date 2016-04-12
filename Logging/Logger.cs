using System;
using System.Collections.Generic;

namespace Logging
{
    public class Logger
    {
        private readonly List<LoggerEventArgs> _logList = new List<LoggerEventArgs>();

        static Logger()
        {
            Singleton = new Logger();
        }

        private Logger()
        {
        }

        public static Logger Singleton { get; }

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

        public event EventHandler<LoggerEventArgs> Log;

        private void AddMsg(LoggingLevel level, string msg)
        {
            var evt = new LoggerEventArgs(level, msg);
            _logList.Add(evt);

            var l = Log; // Keep reference
            if (l != null)
                l(this, evt);
        }

        private void AddMsg(LoggingLevel level, string msg, params object[] arguments)
        {
            AddMsg(level, string.Format(msg, arguments));
        }

        public string GetText()
        {
            return string.Join(Environment.NewLine, _logList);
        }

        public List<LoggerEventArgs> GetEvents()
        {
            return _logList;
        }
    }
}
