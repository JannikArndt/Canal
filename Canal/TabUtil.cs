using System.Windows.Forms;

namespace Canal
{
    public class TabUtil
    {
        private TabControl tabControl;

        public CobolFile CurrentFile
        {
            get
            {
                return tabControl.SelectedTab != null ? ((CodeBox)tabControl.SelectedTab.Controls.Find("CodeBox", false)[0]).CobolFile : null;
            }
        }

        public TabUtil(TabControl tabControl)
        {
            this.tabControl = tabControl;
        }

        public void AddTab(CobolFile file)
        {
            var newTab = new TabPage(file.Name);
            var editor = new CodeBox(file);
            editor.Dock = DockStyle.Fill;
            editor.Name = "CodeBox";
            newTab.Controls.Add(editor);

            tabControl.Controls.Add(newTab);
        }

        public void CloseCurrentTab()
        {
            tabControl.Controls.Remove(tabControl.SelectedTab);
        }
    }
}
