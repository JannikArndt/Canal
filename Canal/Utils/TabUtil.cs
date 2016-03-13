using System.Collections.Generic;
using System.Linq;

namespace Canal.Utils
{
    using System.Drawing;
    using System.Windows.Forms;

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

        private MainWindow _parent;

        public TabUtil(TabControl tabControl, MainWindow parent)
        {
            _parent = parent;
            _tabControl = tabControl;
            _tabControl.DrawMode = TabDrawMode.OwnerDrawFixed;
            _tabControl.DrawItem += _tabControl_DrawItem;
            _tabControl.MouseDown += TabControlOnMouseDown;
        }

        public List<CobolFile> GetOpenFiles()
        {
            var result = new List<CobolFile>();

            foreach (TabPage tab in _tabControl.TabPages)
            {
                result.Add(((FileControl)tab.Controls.Find("FileControl", false)[0]).CobolFile);
            }

            return result;
        }

        private void TabControlOnMouseDown(object sender, MouseEventArgs mouseEventArgs)
        {
            //Looping through the controls.
            for (int i = 0; i < _tabControl.TabPages.Count; i++)
            {
                Rectangle r = _tabControl.GetTabRect(i);
                //Getting the position of the "x" mark.
                Rectangle closeButton = new Rectangle(r.Right - 15, r.Top + 4, 9, 7);
                if (closeButton.Contains(mouseEventArgs.Location))
                {
                    CloseTab(i);
                }
            }
        }

        private void _tabControl_DrawItem(object sender, DrawItemEventArgs e)
        {
            //This code will render a "x" mark at the end of the Tab caption. 
            Font xFont = new Font("Consolas", 10, FontStyle.Bold);

            e.Graphics.DrawString("X", xFont, Brushes.Black, e.Bounds.Right - 15, e.Bounds.Top + 4);
            e.Graphics.DrawString(_tabControl.TabPages[e.Index].Text, e.Font, Brushes.Black, e.Bounds.Left + 12, e.Bounds.Top + 4);
            e.DrawFocusRectangle();
        }

        public void AddTab(CobolFile file)
        {
            var newTab = new TabPage(file.Name + "        ");

            var fileControl = new FileControl(file, _parent)
            {
                Name = "FileControl",
                Dock = DockStyle.Fill
            };

            newTab.Controls.Add(fileControl);
            _tabControl.Controls.Add(newTab);
            _tabControl.SelectTab(newTab);
        }

        public bool CloseTab(int index = -1)
        {
            var tabIndex = index < 0 ? _tabControl.SelectedIndex : index;

            if (MessageBox.Show("Would you like to Close this Tab?", "Confirm", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                _tabControl.TabPages.RemoveAt(tabIndex);
                return true;
            }
            return false;
        }

        public bool CloseAllTabs()
        {
            for (int tabIndex = 0; tabIndex < _tabControl.TabCount; tabIndex++)
            {
                if (!CloseTab(tabIndex))
                    return false;
            }

            return true;
        }

        public TabPage GetCurrentTabPage()
        {
            return _tabControl.SelectedTab;
        }

        public FileControl CurrentFileControl
        {
            get { return (FileControl)_tabControl.Controls.Find("FileControl", false).FirstOrDefault(); }
        }
    }
}
