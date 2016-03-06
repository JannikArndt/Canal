using System.Windows.Forms;

namespace Canal
{
    public class TabUtil
    {
        private readonly TabControl _tabControl;

        // C# 6.0: public CobolFile CurrentFile => ((FileControl) tabControl.SelectedTab?.Controls.Find("FileControl", false)[0])?.CobolFile;

        public CobolFile CurrentFile
        {
            get
            {
                return _tabControl.SelectedTab != null ? ((FileControl)_tabControl.SelectedTab.Controls.Find("FileControl", false)[0]).CobolFile : null;
            }
        }

        public TabUtil(TabControl tabControl)
        {
            this._tabControl = tabControl;
        }

        public void AddTab(CobolFile file)
        {
            var newTab = new TabPage(file.Name);

            var fileControl = new FileControl(file)
            {
                Name = "FileControl",
                Dock = DockStyle.Fill
            };

            newTab.Controls.Add(fileControl);

            _tabControl.Controls.Add(newTab);

            _tabControl.SelectTab(newTab);
        }

        public void CloseCurrentTab()
        {
            _tabControl.Controls.Remove(_tabControl.SelectedTab);
        }
    }
}
