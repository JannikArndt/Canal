using Logging;
using System;
using System.Globalization;
using System.Threading;
using System.Windows.Forms;

namespace Canal
{
    static class Program
    {
        // ReSharper disable once UnusedMember.Local Object has to stay alive
        private static ConsoleLogger _log = new ConsoleLogger();

        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main(string[] args)
        {
            try
            {
                Thread.CurrentThread.CurrentCulture = CultureInfo.InvariantCulture;
                Thread.CurrentThread.CurrentUICulture = CultureInfo.InvariantCulture;
                Application.EnableVisualStyles();
                Application.SetCompatibleTextRenderingDefault(false);
                Application.Run(new MainWindow(args));
            }
            catch (Exception exception)
            {
                Logger.Error("Error in application {0}: {1}", exception.GetType(), exception.Message);
                MessageBox.Show(string.Format("An error occured: {0}.\n\nStack Trace:\n{1}", exception.Message,
                    exception.StackTrace));
            }
        }
    }
}
