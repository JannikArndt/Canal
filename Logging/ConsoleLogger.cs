using System;

namespace Logging
{
    public class ConsoleLogger
    {
        static ConsoleLogger()
        {
            Logger.Singleton.Log += (sender, msg) =>
            {
                Console.WriteLine("{0}:\t{1}", msg.Level.ToString(), msg.Message);
            };
        }
    }
}
