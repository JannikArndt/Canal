using System;
using System.Windows.Forms;

namespace Canal
{
    using Level88ToEnum;
    using Windows;

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
            var dialogResult = openFileDialog.ShowDialog();

            if (dialogResult == DialogResult.OK)
            {
                var file = FileUtil.Get(openFileDialog.FileName);
                file.Name = openFileDialog.FileName;

                tabUtil.AddTab(file);
            }
        }

        private void closeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            tabUtil.CloseCurrentTab();
        }

        private void level88ToEnumConverterToolStripMenuItem_Click(object sender, EventArgs e)
        {
            var converterWindow = new Level88ToEnum();
            converterWindow.Show();
        }

        private void showPERFORMsToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (tabUtil.CurrentFile == null)
                return;

            var performsWindow = new Performs(tabUtil.CurrentFile);
            performsWindow.Show();
        }

        private void exitToolStripMenuItem_Click(object sender, EventArgs e)
        {
            Close();
        }
    }
}
