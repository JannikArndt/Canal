using Canal.Events;
using Canal.Properties;
using System;
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

        private readonly MainWindow _parent;

        public TabUtil(TabControl tabControl, MainWindow parent)
        {
            _parent = parent;
            _tabControl = tabControl;
            _tabControl.DrawMode = TabDrawMode.OwnerDrawFixed;
            _tabControl.DrawItem += _tabControl_DrawItem;
            _tabControl.MouseDown += TabControlOnMouseDown;
        }

        public IEnumerable<FileControl> GetFileControls()
        {
            return (from TabPage tab in _tabControl.TabPages select (FileControl)tab.Controls.Find("FileControl", false)[0]);
        }

        public IEnumerable<CobolFile> GetOpenFiles()
        {
            return GetFileControls().Select(fileControl => fileControl.CobolFile);
        }

        public bool TryShowTab(string filepath)
        {
            try
            {
                foreach (TabPage tab in _tabControl.TabPages)
                {
                    var fileControl = tab.Controls.Find("FileControl", false)[0] as FileControl;

                    if (fileControl != null && fileControl.CobolFile.FileReference.FullPath == filepath)
                    {
                        _tabControl.SelectedTab = tab;
                        return true;
                    }
                }
                return false;

            }
            catch (Exception exception)
            {
                ErrorHandling.Exception(exception);
                return false;
            }
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

            fileControl.UsedFileTypesChanged += UsedFileTypesChanged;

            newTab.Controls.Add(fileControl);
            _tabControl.Controls.Add(newTab);
            _tabControl.SelectTab(newTab);
        }

        private void UsedFileTypesChanged(object sender, UsedFileTypesChangedEventArgs usedFileTypesChangedEventArgs)
        {
            foreach (FileControl fileControl in GetFileControls())
            {
                fileControl.RefreshUsedFileTypes(sender, usedFileTypesChangedEventArgs);
            }
        }

        public bool CloseTab(int index = -1)
        {
            var tabIndex = index < 0 ? _tabControl.SelectedIndex : index;

            if (MessageBox.Show(Resources.ReallyCloseThisTab, Resources.CloseTab, MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                var fileControl = (FileControl)_tabControl.SelectedTab.Controls.Find("FileControl", false)[0];
                // TODO dispose everything
                fileControl.Dispose();
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
