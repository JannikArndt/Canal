using System;

namespace Logging
{
    public class Logger
    {
        static Logger()
        {
            theOnly = new Logger();
        }
        private Logger()
        {
        }

        private static Logger theOnly = null;

        public static Logger Singleton
        {
            get { return theOnly; }
        }

        public event EventHandler<LoggerEventArgs> Log;

        public void AddMsg(int priority, string msg)
        {
            EventHandler<LoggerEventArgs> l = Log;
            if (l != null)
                l(this, new LoggerEventArgs(priority, msg));
        }

        public void AddMsg(int priority, string msg, params object[] arguments)
        {
            AddMsg(priority, string.Format(msg, arguments));
        }
    }
}
