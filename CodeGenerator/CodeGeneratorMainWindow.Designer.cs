namespace CodeGenerator
{
    partial class CodeGeneratorMainWindow
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

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(CodeGeneratorMainWindow));
            this.CodeGeneratorTabControl = new System.Windows.Forms.TabControl();
            this.SettingsTabPage = new System.Windows.Forms.TabPage();
            this.checkedListBox1 = new System.Windows.Forms.CheckedListBox();
            this.ConfigurationTabPage = new System.Windows.Forms.TabPage();
            this.ConfigurationDataGridView = new System.Windows.Forms.DataGridView();
            this.BusinessObjectTabPage = new System.Windows.Forms.TabPage();
            this.BusinessObjectCodeBox = new FastColoredTextBoxNS.FastColoredTextBox();
            this.MapperTabPage = new System.Windows.Forms.TabPage();
            this.MapperCodeBox = new FastColoredTextBoxNS.FastColoredTextBox();
            this.ExtensionMethods = new System.Windows.Forms.TabPage();
            this.ExtensionsCodeBox = new FastColoredTextBoxNS.FastColoredTextBox();
            this.EnumsTabPage = new System.Windows.Forms.TabPage();
            this.EnumsCodeBox = new FastColoredTextBoxNS.FastColoredTextBox();
            this.menuStrip1 = new System.Windows.Forms.MenuStrip();
            this.fileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.editToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.VariableTreeView = new System.Windows.Forms.TreeView();
            this.splitContainer1 = new System.Windows.Forms.SplitContainer();
            this.CodeGeneratorTabControl.SuspendLayout();
            this.SettingsTabPage.SuspendLayout();
            this.ConfigurationTabPage.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.ConfigurationDataGridView)).BeginInit();
            this.BusinessObjectTabPage.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.BusinessObjectCodeBox)).BeginInit();
            this.MapperTabPage.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.MapperCodeBox)).BeginInit();
            this.ExtensionMethods.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.ExtensionsCodeBox)).BeginInit();
            this.EnumsTabPage.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.EnumsCodeBox)).BeginInit();
            this.menuStrip1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer1)).BeginInit();
            this.splitContainer1.Panel1.SuspendLayout();
            this.splitContainer1.Panel2.SuspendLayout();
            this.splitContainer1.SuspendLayout();
            this.SuspendLayout();
            // 
            // CodeGeneratorTabControl
            // 
            this.CodeGeneratorTabControl.Controls.Add(this.SettingsTabPage);
            this.CodeGeneratorTabControl.Controls.Add(this.ConfigurationTabPage);
            this.CodeGeneratorTabControl.Controls.Add(this.BusinessObjectTabPage);
            this.CodeGeneratorTabControl.Controls.Add(this.MapperTabPage);
            this.CodeGeneratorTabControl.Controls.Add(this.ExtensionMethods);
            this.CodeGeneratorTabControl.Controls.Add(this.EnumsTabPage);
            this.CodeGeneratorTabControl.Dock = System.Windows.Forms.DockStyle.Fill;
            this.CodeGeneratorTabControl.Location = new System.Drawing.Point(0, 0);
            this.CodeGeneratorTabControl.Name = "CodeGeneratorTabControl";
            this.CodeGeneratorTabControl.SelectedIndex = 0;
            this.CodeGeneratorTabControl.Size = new System.Drawing.Size(867, 540);
            this.CodeGeneratorTabControl.TabIndex = 0;
            // 
            // SettingsTabPage
            // 
            this.SettingsTabPage.Controls.Add(this.checkedListBox1);
            this.SettingsTabPage.Location = new System.Drawing.Point(4, 22);
            this.SettingsTabPage.Name = "SettingsTabPage";
            this.SettingsTabPage.Padding = new System.Windows.Forms.Padding(3);
            this.SettingsTabPage.Size = new System.Drawing.Size(859, 514);
            this.SettingsTabPage.TabIndex = 5;
            this.SettingsTabPage.Text = "Settings";
            this.SettingsTabPage.UseVisualStyleBackColor = true;
            // 
            // checkedListBox1
            // 
            this.checkedListBox1.FormattingEnabled = true;
            this.checkedListBox1.Location = new System.Drawing.Point(26, 19);
            this.checkedListBox1.Name = "checkedListBox1";
            this.checkedListBox1.Size = new System.Drawing.Size(508, 469);
            this.checkedListBox1.TabIndex = 0;
            // 
            // ConfigurationTabPage
            // 
            this.ConfigurationTabPage.Controls.Add(this.ConfigurationDataGridView);
            this.ConfigurationTabPage.Location = new System.Drawing.Point(4, 22);
            this.ConfigurationTabPage.Name = "ConfigurationTabPage";
            this.ConfigurationTabPage.Padding = new System.Windows.Forms.Padding(3);
            this.ConfigurationTabPage.Size = new System.Drawing.Size(859, 514);
            this.ConfigurationTabPage.TabIndex = 0;
            this.ConfigurationTabPage.Text = "Configuration";
            this.ConfigurationTabPage.UseVisualStyleBackColor = true;
            // 
            // ConfigurationDataGridView
            // 
            this.ConfigurationDataGridView.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.AllCells;
            this.ConfigurationDataGridView.BackgroundColor = System.Drawing.SystemColors.Control;
            this.ConfigurationDataGridView.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.ConfigurationDataGridView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.ConfigurationDataGridView.Location = new System.Drawing.Point(3, 3);
            this.ConfigurationDataGridView.Name = "ConfigurationDataGridView";
            this.ConfigurationDataGridView.RowHeadersBorderStyle = System.Windows.Forms.DataGridViewHeaderBorderStyle.Single;
            this.ConfigurationDataGridView.RowTemplate.Resizable = System.Windows.Forms.DataGridViewTriState.False;
            this.ConfigurationDataGridView.Size = new System.Drawing.Size(853, 508);
            this.ConfigurationDataGridView.TabIndex = 0;
            // 
            // BusinessObjectTabPage
            // 
            this.BusinessObjectTabPage.Controls.Add(this.BusinessObjectCodeBox);
            this.BusinessObjectTabPage.Location = new System.Drawing.Point(4, 22);
            this.BusinessObjectTabPage.Name = "BusinessObjectTabPage";
            this.BusinessObjectTabPage.Padding = new System.Windows.Forms.Padding(3);
            this.BusinessObjectTabPage.Size = new System.Drawing.Size(859, 514);
            this.BusinessObjectTabPage.TabIndex = 1;
            this.BusinessObjectTabPage.Text = "Business Object";
            this.BusinessObjectTabPage.UseVisualStyleBackColor = true;
            this.BusinessObjectTabPage.Paint += new System.Windows.Forms.PaintEventHandler(this.BusinessObjectTabPage_Paint);
            // 
            // BusinessObjectCodeBox
            // 
            this.BusinessObjectCodeBox.AutoCompleteBracketsList = new char[] {
        '(',
        ')',
        '{',
        '}',
        '[',
        ']',
        '\"',
        '\"',
        '\'',
        '\''};
            this.BusinessObjectCodeBox.AutoScrollMinSize = new System.Drawing.Size(47, 14);
            this.BusinessObjectCodeBox.BackBrush = null;
            this.BusinessObjectCodeBox.CharHeight = 14;
            this.BusinessObjectCodeBox.CharWidth = 8;
            this.BusinessObjectCodeBox.Cursor = System.Windows.Forms.Cursors.IBeam;
            this.BusinessObjectCodeBox.DisabledColor = System.Drawing.Color.FromArgb(((int)(((byte)(100)))), ((int)(((byte)(180)))), ((int)(((byte)(180)))), ((int)(((byte)(180)))));
            this.BusinessObjectCodeBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.BusinessObjectCodeBox.Hotkeys = resources.GetString("BusinessObjectCodeBox.Hotkeys");
            this.BusinessObjectCodeBox.IsReplaceMode = false;
            this.BusinessObjectCodeBox.Location = new System.Drawing.Point(3, 3);
            this.BusinessObjectCodeBox.Name = "BusinessObjectCodeBox";
            this.BusinessObjectCodeBox.Paddings = new System.Windows.Forms.Padding(0);
            this.BusinessObjectCodeBox.SelectionColor = System.Drawing.Color.FromArgb(((int)(((byte)(60)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(255)))));
            this.BusinessObjectCodeBox.ServiceColors = ((FastColoredTextBoxNS.ServiceColors)(resources.GetObject("BusinessObjectCodeBox.ServiceColors")));
            this.BusinessObjectCodeBox.Size = new System.Drawing.Size(853, 508);
            this.BusinessObjectCodeBox.TabIndex = 0;
            this.BusinessObjectCodeBox.Zoom = 100;
            // 
            // MapperTabPage
            // 
            this.MapperTabPage.Controls.Add(this.MapperCodeBox);
            this.MapperTabPage.Location = new System.Drawing.Point(4, 22);
            this.MapperTabPage.Name = "MapperTabPage";
            this.MapperTabPage.Padding = new System.Windows.Forms.Padding(3);
            this.MapperTabPage.Size = new System.Drawing.Size(859, 514);
            this.MapperTabPage.TabIndex = 2;
            this.MapperTabPage.Text = "Mapper";
            this.MapperTabPage.UseVisualStyleBackColor = true;
            this.MapperTabPage.Paint += new System.Windows.Forms.PaintEventHandler(this.MapperTabPage_Paint);
            // 
            // MapperCodeBox
            // 
            this.MapperCodeBox.AutoCompleteBracketsList = new char[] {
        '(',
        ')',
        '{',
        '}',
        '[',
        ']',
        '\"',
        '\"',
        '\'',
        '\''};
            this.MapperCodeBox.AutoScrollMinSize = new System.Drawing.Size(47, 14);
            this.MapperCodeBox.BackBrush = null;
            this.MapperCodeBox.CharHeight = 14;
            this.MapperCodeBox.CharWidth = 8;
            this.MapperCodeBox.Cursor = System.Windows.Forms.Cursors.IBeam;
            this.MapperCodeBox.DisabledColor = System.Drawing.Color.FromArgb(((int)(((byte)(100)))), ((int)(((byte)(180)))), ((int)(((byte)(180)))), ((int)(((byte)(180)))));
            this.MapperCodeBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.MapperCodeBox.Hotkeys = resources.GetString("MapperCodeBox.Hotkeys");
            this.MapperCodeBox.IsReplaceMode = false;
            this.MapperCodeBox.Location = new System.Drawing.Point(3, 3);
            this.MapperCodeBox.Name = "MapperCodeBox";
            this.MapperCodeBox.Paddings = new System.Windows.Forms.Padding(0);
            this.MapperCodeBox.SelectionColor = System.Drawing.Color.FromArgb(((int)(((byte)(60)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(255)))));
            this.MapperCodeBox.ServiceColors = ((FastColoredTextBoxNS.ServiceColors)(resources.GetObject("MapperCodeBox.ServiceColors")));
            this.MapperCodeBox.Size = new System.Drawing.Size(853, 508);
            this.MapperCodeBox.TabIndex = 0;
            this.MapperCodeBox.Zoom = 100;
            // 
            // ExtensionMethods
            // 
            this.ExtensionMethods.Controls.Add(this.ExtensionsCodeBox);
            this.ExtensionMethods.Location = new System.Drawing.Point(4, 22);
            this.ExtensionMethods.Name = "ExtensionMethods";
            this.ExtensionMethods.Padding = new System.Windows.Forms.Padding(3);
            this.ExtensionMethods.Size = new System.Drawing.Size(859, 514);
            this.ExtensionMethods.TabIndex = 3;
            this.ExtensionMethods.Text = "Extensions";
            this.ExtensionMethods.UseVisualStyleBackColor = true;
            this.ExtensionMethods.Paint += new System.Windows.Forms.PaintEventHandler(this.ExtensionMethods_Paint);
            // 
            // ExtensionsCodeBox
            // 
            this.ExtensionsCodeBox.AutoCompleteBracketsList = new char[] {
        '(',
        ')',
        '{',
        '}',
        '[',
        ']',
        '\"',
        '\"',
        '\'',
        '\''};
            this.ExtensionsCodeBox.AutoScrollMinSize = new System.Drawing.Size(47, 14);
            this.ExtensionsCodeBox.BackBrush = null;
            this.ExtensionsCodeBox.CharHeight = 14;
            this.ExtensionsCodeBox.CharWidth = 8;
            this.ExtensionsCodeBox.Cursor = System.Windows.Forms.Cursors.IBeam;
            this.ExtensionsCodeBox.DisabledColor = System.Drawing.Color.FromArgb(((int)(((byte)(100)))), ((int)(((byte)(180)))), ((int)(((byte)(180)))), ((int)(((byte)(180)))));
            this.ExtensionsCodeBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.ExtensionsCodeBox.Hotkeys = resources.GetString("ExtensionsCodeBox.Hotkeys");
            this.ExtensionsCodeBox.IsReplaceMode = false;
            this.ExtensionsCodeBox.Location = new System.Drawing.Point(3, 3);
            this.ExtensionsCodeBox.Name = "ExtensionsCodeBox";
            this.ExtensionsCodeBox.Paddings = new System.Windows.Forms.Padding(0);
            this.ExtensionsCodeBox.SelectionColor = System.Drawing.Color.FromArgb(((int)(((byte)(60)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(255)))));
            this.ExtensionsCodeBox.ServiceColors = ((FastColoredTextBoxNS.ServiceColors)(resources.GetObject("ExtensionsCodeBox.ServiceColors")));
            this.ExtensionsCodeBox.Size = new System.Drawing.Size(853, 508);
            this.ExtensionsCodeBox.TabIndex = 0;
            this.ExtensionsCodeBox.Zoom = 100;
            // 
            // EnumsTabPage
            // 
            this.EnumsTabPage.Controls.Add(this.EnumsCodeBox);
            this.EnumsTabPage.Location = new System.Drawing.Point(4, 22);
            this.EnumsTabPage.Name = "EnumsTabPage";
            this.EnumsTabPage.Padding = new System.Windows.Forms.Padding(3);
            this.EnumsTabPage.Size = new System.Drawing.Size(859, 514);
            this.EnumsTabPage.TabIndex = 4;
            this.EnumsTabPage.Text = "Enums";
            this.EnumsTabPage.UseVisualStyleBackColor = true;
            this.EnumsTabPage.Paint += new System.Windows.Forms.PaintEventHandler(this.EnumsTabPage_Paint);
            // 
            // EnumsCodeBox
            // 
            this.EnumsCodeBox.AutoCompleteBracketsList = new char[] {
        '(',
        ')',
        '{',
        '}',
        '[',
        ']',
        '\"',
        '\"',
        '\'',
        '\''};
            this.EnumsCodeBox.AutoScrollMinSize = new System.Drawing.Size(47, 14);
            this.EnumsCodeBox.BackBrush = null;
            this.EnumsCodeBox.CharHeight = 14;
            this.EnumsCodeBox.CharWidth = 8;
            this.EnumsCodeBox.Cursor = System.Windows.Forms.Cursors.IBeam;
            this.EnumsCodeBox.DisabledColor = System.Drawing.Color.FromArgb(((int)(((byte)(100)))), ((int)(((byte)(180)))), ((int)(((byte)(180)))), ((int)(((byte)(180)))));
            this.EnumsCodeBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.EnumsCodeBox.Hotkeys = resources.GetString("EnumsCodeBox.Hotkeys");
            this.EnumsCodeBox.IsReplaceMode = false;
            this.EnumsCodeBox.Location = new System.Drawing.Point(3, 3);
            this.EnumsCodeBox.Name = "EnumsCodeBox";
            this.EnumsCodeBox.Paddings = new System.Windows.Forms.Padding(0);
            this.EnumsCodeBox.SelectionColor = System.Drawing.Color.FromArgb(((int)(((byte)(60)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(255)))));
            this.EnumsCodeBox.ServiceColors = ((FastColoredTextBoxNS.ServiceColors)(resources.GetObject("EnumsCodeBox.ServiceColors")));
            this.EnumsCodeBox.Size = new System.Drawing.Size(853, 508);
            this.EnumsCodeBox.TabIndex = 0;
            this.EnumsCodeBox.Zoom = 100;
            // 
            // menuStrip1
            // 
            this.menuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.fileToolStripMenuItem,
            this.editToolStripMenuItem});
            this.menuStrip1.Location = new System.Drawing.Point(0, 0);
            this.menuStrip1.Name = "menuStrip1";
            this.menuStrip1.Size = new System.Drawing.Size(1071, 24);
            this.menuStrip1.TabIndex = 1;
            this.menuStrip1.Text = "menuStrip1";
            // 
            // fileToolStripMenuItem
            // 
            this.fileToolStripMenuItem.Name = "fileToolStripMenuItem";
            this.fileToolStripMenuItem.Size = new System.Drawing.Size(37, 20);
            this.fileToolStripMenuItem.Text = "File";
            // 
            // editToolStripMenuItem
            // 
            this.editToolStripMenuItem.Name = "editToolStripMenuItem";
            this.editToolStripMenuItem.Size = new System.Drawing.Size(39, 20);
            this.editToolStripMenuItem.Text = "Edit";
            // 
            // VariableTreeView
            // 
            this.VariableTreeView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.VariableTreeView.Location = new System.Drawing.Point(0, 0);
            this.VariableTreeView.Name = "VariableTreeView";
            this.VariableTreeView.Size = new System.Drawing.Size(200, 540);
            this.VariableTreeView.TabIndex = 2;
            this.VariableTreeView.AfterSelect += new System.Windows.Forms.TreeViewEventHandler(this.VariableTreeView_AfterSelect);
            // 
            // splitContainer1
            // 
            this.splitContainer1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer1.FixedPanel = System.Windows.Forms.FixedPanel.Panel1;
            this.splitContainer1.Location = new System.Drawing.Point(0, 24);
            this.splitContainer1.Name = "splitContainer1";
            // 
            // splitContainer1.Panel1
            // 
            this.splitContainer1.Panel1.Controls.Add(this.VariableTreeView);
            // 
            // splitContainer1.Panel2
            // 
            this.splitContainer1.Panel2.Controls.Add(this.CodeGeneratorTabControl);
            this.splitContainer1.Size = new System.Drawing.Size(1071, 540);
            this.splitContainer1.SplitterDistance = 200;
            this.splitContainer1.TabIndex = 3;
            // 
            // CodeGeneratorMainWindow
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1071, 564);
            this.Controls.Add(this.splitContainer1);
            this.Controls.Add(this.menuStrip1);
            this.MainMenuStrip = this.menuStrip1;
            this.Name = "CodeGeneratorMainWindow";
            this.Text = "Canal Code Generator";
            this.CodeGeneratorTabControl.ResumeLayout(false);
            this.SettingsTabPage.ResumeLayout(false);
            this.ConfigurationTabPage.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.ConfigurationDataGridView)).EndInit();
            this.BusinessObjectTabPage.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.BusinessObjectCodeBox)).EndInit();
            this.MapperTabPage.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.MapperCodeBox)).EndInit();
            this.ExtensionMethods.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.ExtensionsCodeBox)).EndInit();
            this.EnumsTabPage.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.EnumsCodeBox)).EndInit();
            this.menuStrip1.ResumeLayout(false);
            this.menuStrip1.PerformLayout();
            this.splitContainer1.Panel1.ResumeLayout(false);
            this.splitContainer1.Panel2.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer1)).EndInit();
            this.splitContainer1.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TabControl CodeGeneratorTabControl;
        private System.Windows.Forms.TabPage ConfigurationTabPage;
        private System.Windows.Forms.TabPage BusinessObjectTabPage;
        private System.Windows.Forms.DataGridView ConfigurationDataGridView;
        private System.Windows.Forms.TabPage MapperTabPage;
        private System.Windows.Forms.TabPage ExtensionMethods;
        private System.Windows.Forms.TabPage EnumsTabPage;
        private FastColoredTextBoxNS.FastColoredTextBox BusinessObjectCodeBox;
        private FastColoredTextBoxNS.FastColoredTextBox MapperCodeBox;
        private FastColoredTextBoxNS.FastColoredTextBox ExtensionsCodeBox;
        private FastColoredTextBoxNS.FastColoredTextBox EnumsCodeBox;
        private System.Windows.Forms.MenuStrip menuStrip1;
        private System.Windows.Forms.ToolStripMenuItem fileToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem editToolStripMenuItem;
        private System.Windows.Forms.TabPage SettingsTabPage;
        private System.Windows.Forms.CheckedListBox checkedListBox1;
        private System.Windows.Forms.TreeView VariableTreeView;
        private System.Windows.Forms.SplitContainer splitContainer1;
    }
}

