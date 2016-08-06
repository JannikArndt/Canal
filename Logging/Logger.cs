using System.Linq;

namespace Logging
{
    using System;
    using System.Collections.Generic;

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

        public event EventHandler<LoggerEventArgs> All;

        public event EventHandler<LoggerEventArgs> WarningsAndErrors;

        public event EventHandler<LoggerEventArgs> Errors;

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

        public static void Error(Exception exception, string msg = "", params object[] arguments)
        {
            var message = string.Format(msg, arguments);
            var exceptionString = string.Format("{0}: {1}\n{2}", exception.GetType(), exception.Message, exception.StackTrace);
            Singleton.AddMsg(LoggingLevel.Error, message + exceptionString);
        }

        public IEnumerable<LoggerEventArgs> GetEvents(LoggingLevel level = LoggingLevel.Info, int i = -1)
        {
            if (i < 0)
                return _logList.Where(evt => evt.Level >= level);


            var evts = _logList.Where(evt => evt.Level >= level).ToList();
            return evts.Skip(evts.Count - i);
        }

        public bool HasWarnings()
        {
            return _logList.Any(evt => evt.Level == LoggingLevel.Warning);
        }

        public bool HasErrors()
        {
            return _logList.Any(evt => evt.Level == LoggingLevel.Error);
        }

        public bool HasWarningsOrErrors()
        {
            return _logList.Any(evt => evt.Level == LoggingLevel.Warning || evt.Level == LoggingLevel.Error);
        }

        private void AddMsg(LoggingLevel level, string msg)
        {
            var evt = new LoggerEventArgs(level, msg);
            _logList.Add(evt);

            var l = All; // Keep reference
            if (l != null)
                l(this, evt);

            if (WarningsAndErrors != null && evt.Level > LoggingLevel.Info)
                WarningsAndErrors(this, evt);

            if (Errors != null && evt.Level == LoggingLevel.Error)
                Errors(this, evt);
        }

        private void AddMsg(LoggingLevel level, string msg, params object[] arguments)
        {
            AddMsg(level, string.Format(msg, arguments));
        }
    }
}
