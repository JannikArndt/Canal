using Logging;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;

namespace Canal
{
    public partial class Log : Form
    {
        private readonly List<LoggerEventArgs> _eventList;

        private LoggingLevel _currentFilter = LoggingLevel.Info;

        public Log()
        {
            InitializeComponent();

            _eventList = Logger.Singleton.GetEvents();
            Logger.Singleton.Log += OnLog;

            levelFilter.DataSource = Enum.GetValues(typeof(LoggingLevel));

        }

        private void OnLog(object sender, LoggerEventArgs args)
        {
            if (args.Level < _currentFilter) return;

            if (logTextBox.InvokeRequired)
                logTextBox.Invoke(new MethodInvoker(UpdateList));
            else
                UpdateList();
        }

        private void comboBox1_SelectedIndexChanged(object sender, EventArgs e)
        {
            _currentFilter = (LoggingLevel)levelFilter.SelectedIndex;

            logTextBox.Text = string.Join(Environment.NewLine, _eventList.Where(evt => evt.Level >= _currentFilter));
            logTextBox.AppendText(Environment.NewLine);
        }

        private void UpdateList()
        {
            if (logTextBox.IsDisposed)
                return;

            logTextBox.AppendText(_eventList.Last() + Environment.NewLine);
        }
    }
}
