namespace Canal.UserControls
{
    partial class FilesView
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(FilesView));
            this.filesTabToolStrip = new System.Windows.Forms.ToolStrip();
            this.filesTabSearchBox = new System.Windows.Forms.ToolStripTextBox();
            this.fileTypeDropDownButton = new System.Windows.Forms.ToolStripDropDownButton();
            this.showFileTypes_cob = new System.Windows.Forms.ToolStripMenuItem();
            this.showFileTypes_txt = new System.Windows.Forms.ToolStripMenuItem();
            this.showFileTypes_src = new System.Windows.Forms.ToolStripMenuItem();
            this.showFileTypes_custom = new System.Windows.Forms.ToolStripTextBox();
            this.CollapseAllToolStripButton = new System.Windows.Forms.ToolStripButton();
            this.ExpandAllToolStripButton = new System.Windows.Forms.ToolStripButton();
            this.filesTreeView = new System.Windows.Forms.TreeView();
            this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.filesTabToolStrip.SuspendLayout();
            this.SuspendLayout();
            // 
            // filesTabToolStrip
            // 
            this.filesTabToolStrip.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
            this.filesTabToolStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.filesTabSearchBox,
            this.fileTypeDropDownButton,
            this.toolStripSeparator1,
            this.CollapseAllToolStripButton,
            this.ExpandAllToolStripButton});
            this.filesTabToolStrip.Location = new System.Drawing.Point(0, 0);
            this.filesTabToolStrip.Name = "filesTabToolStrip";
            this.filesTabToolStrip.Size = new System.Drawing.Size(381, 25);
            this.filesTabToolStrip.TabIndex = 2;
            this.filesTabToolStrip.Text = "toolStrip1";
            // 
            // filesTabSearchBox
            // 
            this.filesTabSearchBox.Name = "filesTabSearchBox";
            this.filesTabSearchBox.Size = new System.Drawing.Size(100, 25);
            this.filesTabSearchBox.Text = "Search Files...";
            this.filesTabSearchBox.Enter += new System.EventHandler(this.SearchBoxEnter);
            this.filesTabSearchBox.Leave += new System.EventHandler(this.SearchBoxLeave);
            this.filesTabSearchBox.TextChanged += new System.EventHandler(this.FilesTabSearchBoxTextChanged);
            // 
            // fileTypeDropDownButton
            // 
            this.fileTypeDropDownButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.fileTypeDropDownButton.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.showFileTypes_cob,
            this.showFileTypes_txt,
            this.showFileTypes_src,
            this.showFileTypes_custom});
            this.fileTypeDropDownButton.Image = ((System.Drawing.Image)(resources.GetObject("fileTypeDropDownButton.Image")));
            this.fileTypeDropDownButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.fileTypeDropDownButton.Name = "fileTypeDropDownButton";
            this.fileTypeDropDownButton.Size = new System.Drawing.Size(29, 22);
            this.fileTypeDropDownButton.Text = "File Types";
            // 
            // showFileTypes_cob
            // 
            this.showFileTypes_cob.Checked = true;
            this.showFileTypes_cob.CheckOnClick = true;
            this.showFileTypes_cob.CheckState = System.Windows.Forms.CheckState.Checked;
            this.showFileTypes_cob.Name = "showFileTypes_cob";
            this.showFileTypes_cob.Size = new System.Drawing.Size(160, 22);
            this.showFileTypes_cob.Text = "*.cob/*.cbl";
            this.showFileTypes_cob.Click += new System.EventHandler(this.FileTypeClicked);
            // 
            // showFileTypes_txt
            // 
            this.showFileTypes_txt.CheckOnClick = true;
            this.showFileTypes_txt.Name = "showFileTypes_txt";
            this.showFileTypes_txt.Size = new System.Drawing.Size(160, 22);
            this.showFileTypes_txt.Text = "*.txt";
            this.showFileTypes_txt.Click += new System.EventHandler(this.FileTypeClicked);
            // 
            // showFileTypes_src
            // 
            this.showFileTypes_src.CheckOnClick = true;
            this.showFileTypes_src.Name = "showFileTypes_src";
            this.showFileTypes_src.Size = new System.Drawing.Size(160, 22);
            this.showFileTypes_src.Text = "*.src";
            this.showFileTypes_src.Click += new System.EventHandler(this.FileTypeClicked);
            // 
            // showFileTypes_custom
            // 
            this.showFileTypes_custom.Name = "showFileTypes_custom";
            this.showFileTypes_custom.Size = new System.Drawing.Size(100, 23);
            // 
            // CollapseAllToolStripButton
            // 
            this.CollapseAllToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.CollapseAllToolStripButton.Image = global::Canal.Properties.Resources.CollapseAllButton_Image;
            this.CollapseAllToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.CollapseAllToolStripButton.Name = "CollapseAllToolStripButton";
            this.CollapseAllToolStripButton.Size = new System.Drawing.Size(23, 22);
            this.CollapseAllToolStripButton.Text = "Collapse All";
            this.CollapseAllToolStripButton.Click += new System.EventHandler(this.CollapseAllToolStripButton_Click);
            // 
            // ExpandAllToolStripButton
            // 
            this.ExpandAllToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.ExpandAllToolStripButton.Image = global::Canal.Properties.Resources.ExpandAllButton_Image;
            this.ExpandAllToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.ExpandAllToolStripButton.Name = "ExpandAllToolStripButton";
            this.ExpandAllToolStripButton.Size = new System.Drawing.Size(23, 22);
            this.ExpandAllToolStripButton.Text = "Expand All";
            this.ExpandAllToolStripButton.Click += new System.EventHandler(this.ExpandAllToolStripButton_Click);
            // 
            // filesTreeView
            // 
            this.filesTreeView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.filesTreeView.Location = new System.Drawing.Point(0, 25);
            this.filesTreeView.Name = "filesTreeView";
            this.filesTreeView.Size = new System.Drawing.Size(381, 739);
            this.filesTreeView.TabIndex = 3;
            this.filesTreeView.DoubleClick += new System.EventHandler(this.FilesTreeViewDoubleClick);
            this.filesTreeView.KeyUp += new System.Windows.Forms.KeyEventHandler(this.FilesTreeViewKeyUp);
            // 
            // toolStripSeparator1
            // 
            this.toolStripSeparator1.Name = "toolStripSeparator1";
            this.toolStripSeparator1.Size = new System.Drawing.Size(6, 25);
            // 
            // FilesView
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.filesTreeView);
            this.Controls.Add(this.filesTabToolStrip);
            this.Name = "FilesView";
            this.Size = new System.Drawing.Size(381, 764);
            this.filesTabToolStrip.ResumeLayout(false);
            this.filesTabToolStrip.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.ToolStrip filesTabToolStrip;
        private System.Windows.Forms.ToolStripTextBox filesTabSearchBox;
        private System.Windows.Forms.ToolStripDropDownButton fileTypeDropDownButton;
        private System.Windows.Forms.ToolStripMenuItem showFileTypes_cob;
        private System.Windows.Forms.ToolStripMenuItem showFileTypes_txt;
        private System.Windows.Forms.ToolStripMenuItem showFileTypes_src;
        private System.Windows.Forms.ToolStripTextBox showFileTypes_custom;
        private System.Windows.Forms.TreeView filesTreeView;
        private System.Windows.Forms.ToolStripButton CollapseAllToolStripButton;
        private System.Windows.Forms.ToolStripButton ExpandAllToolStripButton;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
    }
}
