using Canal.Utils;
using System;
using System.Windows.Forms;

namespace Canal
{
    static class Program
    {

        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main(string[] args)
        {
            try
            {
                Application.EnableVisualStyles();
                Application.SetCompatibleTextRenderingDefault(false);
                Application.Run(new MainWindow(args));
            }
            catch (Exception exception)
            {
                ErrorHandling.Exception(exception);
            }
        }
    }
}
