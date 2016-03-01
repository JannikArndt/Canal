using System;
using System.Windows.Forms;

namespace Canal
{
    using _88ToEnum;

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
            var dialogResult = openFileDialog1.ShowDialog();

            if (dialogResult == DialogResult.OK)
            {
                var file = FileUtil.Get(openFileDialog1.FileName);
                file.Name = openFileDialog1.FileName;

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
    }
}
