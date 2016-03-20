using Mixpanel;
using System;

namespace Canal.Utils
{
    public static class ErrorHandling
    {
        public static void Start()
        {
            Track("Start");
        }

        public static void End()
        {
            Track("End");
        }

        public static void Exception(Exception exception)
        {
            Track("Exception", exception);
        }

        public static void Track(string name, Exception exception = null)
        {
            var client = MixpanelClient.GetCurrentClient();
            var ev = new TrackingEvent(name)
            {
                Properties = new TrackingEventProperties
                {
                    Token = "a12a731feb52dc0d468c428d7a814b89",
                    DistinctId = Environment.UserName
                }
            };

            if (exception != null)
            {
                ev.Properties.All["ExceptionType"] = exception.GetType();
                ev.Properties.All["ErrorMessage"] = exception.Message;
                ev.Properties.All["Exception"] = exception.ToString();
                ev.Properties.All["StackTrace"] = exception.StackTrace;
            }

            client.Track(ev);
        }
    }
}
