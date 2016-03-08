using System;
using System.Windows.Forms;

namespace Canal
{
    using Canal.Utils;
    using Level88ToEnum;
    using System.Text;

    public partial class MainWindow : Form
    {
        private TabUtil tabUtil;

        public MainWindow(string[] files = null)
        {
            InitializeComponent();

            tabUtil = new TabUtil(FileTabs);

            if (files != null)
            {
                foreach (var filename in files)
                {
                    var file = FileUtil.Get(filename);
                    file.Name = filename;
                    tabUtil.AddTab(file);
                }
            }
        }

        private void openToolStripMenuItem_Click(object sender, EventArgs e)
        {
            openFileDialog.Filter = @"COBOL Files|*.cob;*.cbl;*.txt";
            openFileDialog.FileName = "";

            var dialogResult = openFileDialog.ShowDialog();

            if (dialogResult == DialogResult.OK)
            {
                Cursor = Cursors.WaitCursor;
                var file = FileUtil.Get(openFileDialog.FileName);
                tabUtil.AddTab(file);
                Cursor = Cursors.Default;
            }
        }

        private void closeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            tabUtil.CloseTab();
        }

        private void level88ToEnumConverterToolStripMenuItem_Click(object sender, EventArgs e)
        {
            var converterWindow = new Level88ToEnum();
            converterWindow.Show();
        }

        private void exitToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (tabUtil.CloseAllTabs())
                Close();
        }

        private void variableUsagesToolStripMenuItem_Click(object sender, EventArgs e)
        {
            var text = new StringBuilder();

            foreach (var section in tabUtil.CurrentFile.CobolTree.ProcedureDivision.Sections)
            {
                text.AppendLine("Section: " + section.Name);

                foreach (var procedure in section.Procedures)
                {
                    text.AppendLine("Procedure: " + procedure.Name);

                    foreach (var variable in procedure.Variables.Keys)
                    {
                        var root = variable.Root != null ? " (" + variable.Root.Name + ")" : "";
                        text.AppendLine("  " + variable.Level.ToString("D2") + "  " + variable.Name + root + " " + procedure.Variables[variable].ToShortString());
                    }

                    text.AppendLine();
                }

                text.AppendLine();
            }

            var viewer = new TextViewer(text.ToString());
            viewer.Show();
        }
    }
}
