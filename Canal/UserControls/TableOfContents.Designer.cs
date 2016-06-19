namespace Canal.UserControls
{
    partial class TableOfContents
    {
        /// <summary> 
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(TableOfContents));
            this.tocToolStrip = new System.Windows.Forms.ToolStrip();
            this.TocCopyButton = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator6 = new System.Windows.Forms.ToolStripSeparator();
            this.TocCollapseAllButton = new System.Windows.Forms.ToolStripButton();
            this.TocExpandAllButton = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator5 = new System.Windows.Forms.ToolStripSeparator();
            this.TocSortHierarchicallyButton = new System.Windows.Forms.ToolStripButton();
            this.TocSortAlphabeticallyButton = new System.Windows.Forms.ToolStripButton();
            this.TocSortByPerformsButton = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator7 = new System.Windows.Forms.ToolStripSeparator();
            this.tocSearchTextBox = new System.Windows.Forms.ToolStripTextBox();
            this.tocTreeView = new System.Windows.Forms.TreeView();
            this.tocToolStrip.SuspendLayout();
            this.SuspendLayout();
            // 
            // tocToolStrip
            // 
            this.tocToolStrip.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
            this.tocToolStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.TocCopyButton,
            this.toolStripSeparator6,
            this.TocCollapseAllButton,
            this.TocExpandAllButton,
            this.toolStripSeparator5,
            this.TocSortHierarchicallyButton,
            this.TocSortAlphabeticallyButton,
            this.TocSortByPerformsButton,
            this.toolStripSeparator7,
            this.tocSearchTextBox});
            this.tocToolStrip.Location = new System.Drawing.Point(0, 0);
            this.tocToolStrip.Name = "tocToolStrip";
            this.tocToolStrip.Size = new System.Drawing.Size(428, 25);
            this.tocToolStrip.TabIndex = 3;
            this.tocToolStrip.Text = "toolStrip1";
            // 
            // TocCopyButton
            // 
            this.TocCopyButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.TocCopyButton.Image = ((System.Drawing.Image)(resources.GetObject("TocCopyButton.Image")));
            this.TocCopyButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.TocCopyButton.Name = "TocCopyButton";
            this.TocCopyButton.Size = new System.Drawing.Size(23, 22);
            this.TocCopyButton.Text = "Copy";
            this.TocCopyButton.Click += new System.EventHandler(this.ExportTocClick);
            // 
            // toolStripSeparator6
            // 
            this.toolStripSeparator6.Name = "toolStripSeparator6";
            this.toolStripSeparator6.Size = new System.Drawing.Size(6, 25);
            // 
            // TocCollapseAllButton
            // 
            this.TocCollapseAllButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.TocCollapseAllButton.Image = ((System.Drawing.Image)(resources.GetObject("TocCollapseAllButton.Image")));
            this.TocCollapseAllButton.ImageTransparentColor = System.Drawing.Color.Black;
            this.TocCollapseAllButton.Name = "TocCollapseAllButton";
            this.TocCollapseAllButton.Size = new System.Drawing.Size(23, 22);
            this.TocCollapseAllButton.Text = "Collapse All";
            this.TocCollapseAllButton.Click += new System.EventHandler(this.TocCollapseAllButtonClick);
            // 
            // TocExpandAllButton
            // 
            this.TocExpandAllButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.TocExpandAllButton.Image = ((System.Drawing.Image)(resources.GetObject("TocExpandAllButton.Image")));
            this.TocExpandAllButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.TocExpandAllButton.Name = "TocExpandAllButton";
            this.TocExpandAllButton.Size = new System.Drawing.Size(23, 22);
            this.TocExpandAllButton.Text = "Expand all";
            this.TocExpandAllButton.Click += new System.EventHandler(this.TocExpandAllButtonClick);
            // 
            // toolStripSeparator5
            // 
            this.toolStripSeparator5.Name = "toolStripSeparator5";
            this.toolStripSeparator5.Size = new System.Drawing.Size(6, 25);
            // 
            // TocSortHierarchicallyButton
            // 
            this.TocSortHierarchicallyButton.Checked = true;
            this.TocSortHierarchicallyButton.CheckState = System.Windows.Forms.CheckState.Checked;
            this.TocSortHierarchicallyButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.TocSortHierarchicallyButton.Image = ((System.Drawing.Image)(resources.GetObject("TocSortHierarchicallyButton.Image")));
            this.TocSortHierarchicallyButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.TocSortHierarchicallyButton.Name = "TocSortHierarchicallyButton";
            this.TocSortHierarchicallyButton.Size = new System.Drawing.Size(23, 22);
            this.TocSortHierarchicallyButton.Text = "Sort by Sections";
            this.TocSortHierarchicallyButton.Click += new System.EventHandler(this.TocSortBySectionsButtonClick);
            // 
            // TocSortAlphabeticallyButton
            // 
            this.TocSortAlphabeticallyButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.TocSortAlphabeticallyButton.Image = ((System.Drawing.Image)(resources.GetObject("TocSortAlphabeticallyButton.Image")));
            this.TocSortAlphabeticallyButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.TocSortAlphabeticallyButton.Name = "TocSortAlphabeticallyButton";
            this.TocSortAlphabeticallyButton.Size = new System.Drawing.Size(23, 22);
            this.TocSortAlphabeticallyButton.Text = "Sort Alphabetically";
            this.TocSortAlphabeticallyButton.Click += new System.EventHandler(this.TocSortAlphabeticallyButton_Click);
            // 
            // TocSortByPerformsButton
            // 
            this.TocSortByPerformsButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.TocSortByPerformsButton.Image = ((System.Drawing.Image)(resources.GetObject("TocSortByPerformsButton.Image")));
            this.TocSortByPerformsButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.TocSortByPerformsButton.Name = "TocSortByPerformsButton";
            this.TocSortByPerformsButton.Size = new System.Drawing.Size(23, 22);
            this.TocSortByPerformsButton.Text = "Sort by Performs";
            this.TocSortByPerformsButton.Click += new System.EventHandler(this.TocSortByPerformsButtonClick);
            // 
            // toolStripSeparator7
            // 
            this.toolStripSeparator7.Name = "toolStripSeparator7";
            this.toolStripSeparator7.Size = new System.Drawing.Size(6, 25);
            // 
            // tocSearchTextBox
            // 
            this.tocSearchTextBox.Name = "tocSearchTextBox";
            this.tocSearchTextBox.Size = new System.Drawing.Size(100, 25);
            this.tocSearchTextBox.Enter += new System.EventHandler(this.SearchBoxEnter);
            this.tocSearchTextBox.Leave += new System.EventHandler(this.SearchBoxLeave);
            this.tocSearchTextBox.TextChanged += new System.EventHandler(this.TocSearchTextBox_TextChanged);
            // 
            // tocTreeView
            // 
            this.tocTreeView.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.tocTreeView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tocTreeView.Font = new System.Drawing.Font("Consolas", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.tocTreeView.ItemHeight = 22;
            this.tocTreeView.Location = new System.Drawing.Point(0, 25);
            this.tocTreeView.Margin = new System.Windows.Forms.Padding(2);
            this.tocTreeView.Name = "tocTreeView";
            this.tocTreeView.Size = new System.Drawing.Size(428, 305);
            this.tocTreeView.TabIndex = 6;
            this.tocTreeView.AfterSelect += new System.Windows.Forms.TreeViewEventHandler(this.tocTreeView_AfterSelect);
            // 
            // TableOfContents
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.tocTreeView);
            this.Controls.Add(this.tocToolStrip);
            this.Name = "TableOfContents";
            this.Size = new System.Drawing.Size(428, 330);
            this.tocToolStrip.ResumeLayout(false);
            this.tocToolStrip.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.ToolStrip tocToolStrip;
        private System.Windows.Forms.ToolStripButton TocCopyButton;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator6;
        private System.Windows.Forms.ToolStripButton TocCollapseAllButton;
        private System.Windows.Forms.ToolStripButton TocExpandAllButton;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator5;
        private System.Windows.Forms.ToolStripButton TocSortHierarchicallyButton;
        private System.Windows.Forms.ToolStripButton TocSortAlphabeticallyButton;
        private System.Windows.Forms.ToolStripButton TocSortByPerformsButton;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator7;
        private System.Windows.Forms.ToolStripTextBox tocSearchTextBox;
        private System.Windows.Forms.TreeView tocTreeView;
    }
}
