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
                return this._tabControl.SelectedTab != null ? ((FileControl)this._tabControl.SelectedTab.Controls.Find("FileControl", false)[0]).CobolFile : null;
            }
        }

        public TabUtil(TabControl tabControl)
        {
            this._tabControl = tabControl;
            this._tabControl.DrawMode = TabDrawMode.OwnerDrawFixed;
            this._tabControl.DrawItem += this._tabControl_DrawItem;
            this._tabControl.MouseDown += this.TabControlOnMouseDown;
        }

        private void TabControlOnMouseDown(object sender, MouseEventArgs mouseEventArgs)
        {
            //Looping through the controls.
            for (int i = 0; i < this._tabControl.TabPages.Count; i++)
            {
                Rectangle r = this._tabControl.GetTabRect(i);
                //Getting the position of the "x" mark.
                Rectangle closeButton = new Rectangle(r.Right - 15, r.Top + 4, 9, 7);
                if (closeButton.Contains(mouseEventArgs.Location))
                {
                    this.CloseTab(i);
                }
            }
        }

        private void _tabControl_DrawItem(object sender, DrawItemEventArgs e)
        {
            //This code will render a "x" mark at the end of the Tab caption. 
            Font xFont = new Font("Consolas", 10, FontStyle.Bold);

            e.Graphics.DrawString("X", xFont, Brushes.Black, e.Bounds.Right - 15, e.Bounds.Top + 4);
            e.Graphics.DrawString(this._tabControl.TabPages[e.Index].Text, e.Font, Brushes.Black, e.Bounds.Left + 12, e.Bounds.Top + 4);
            e.DrawFocusRectangle();
        }

        public void AddTab(CobolFile file)
        {
            var newTab = new TabPage(file.Name + "        ");

            var fileControl = new FileControl(file)
            {
                Name = "FileControl",
                Dock = DockStyle.Fill
            };

            newTab.Controls.Add(fileControl);
            this._tabControl.Controls.Add(newTab);
            this._tabControl.SelectTab(newTab);
        }

        public bool CloseTab(int index = -1)
        {
            var tabIndex = index < 0 ? this._tabControl.SelectedIndex : index;

            if (MessageBox.Show("Would you like to Close this Tab?", "Confirm", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                this._tabControl.TabPages.RemoveAt(tabIndex);
                return true;
            }
            return false;
        }

        public bool CloseAllTabs()
        {
            for (int tabIndex = 0; tabIndex < this._tabControl.TabCount; tabIndex++)
            {
                if (!this.CloseTab(tabIndex))
                    return false;
            }

            return true;
        }
    }
}
