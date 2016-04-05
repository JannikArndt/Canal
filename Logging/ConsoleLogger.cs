using Logging;
using System;
using System.Collections.Generic;

namespace Canal.Utils
{
    public class ConsoleLogger
    {
        private static List<string> _logList = new List<string>();

        static ConsoleLogger()
        {
            Logger.Singleton.Log += (sender, msg) =>
            {
                var text = string.Format("{0}:\t{1}", msg.Priority.ToString(), msg.Message);
                _logList.Add(text);
                Console.WriteLine(text);
            };
        }

        public static string GetText()
        {
            return string.Join(Environment.NewLine, _logList);
        }
    }
}
