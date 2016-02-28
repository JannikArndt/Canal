using System.Windows.Forms;

namespace Canal
{
    public partial class MainWindow : Form
    {
        public MainWindow()
        {
            InitializeComponent();

            var file = FileUtil.Get("../../../CodeSamples/RepWriteSumm.cbl");

            var newTab = new TabPage("RepWriteSumm.cbl");
            var editor = new CodeBox(file);
            editor.Dock = DockStyle.Fill;
            newTab.Controls.Add(editor);

            FileTabs.Controls.Add(newTab);
        }
    }
}
