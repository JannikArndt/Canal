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
            this.ConfigurationTabPage = new System.Windows.Forms.TabPage();
            this.ConfigurationDataGridView = new System.Windows.Forms.DataGridView();
            this.BusinessObjectTabPage = new System.Windows.Forms.TabPage();
            this.MapperTabPage = new System.Windows.Forms.TabPage();
            this.ExtensionMethods = new System.Windows.Forms.TabPage();
            this.EnumsTabPage = new System.Windows.Forms.TabPage();
            this.BusinessObjectCodeBox = new FastColoredTextBoxNS.FastColoredTextBox();
            this.MapperCodeBox = new FastColoredTextBoxNS.FastColoredTextBox();
            this.ExtensionsCodeBox = new FastColoredTextBoxNS.FastColoredTextBox();
            this.EnumsCodeBox = new FastColoredTextBoxNS.FastColoredTextBox();
            this.menuStrip1 = new System.Windows.Forms.MenuStrip();
            this.fileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.editToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStrip1 = new System.Windows.Forms.ToolStrip();
            this.CodeGeneratorTabControl.SuspendLayout();
            this.ConfigurationTabPage.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.ConfigurationDataGridView)).BeginInit();
            this.BusinessObjectTabPage.SuspendLayout();
            this.MapperTabPage.SuspendLayout();
            this.ExtensionMethods.SuspendLayout();
            this.EnumsTabPage.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.BusinessObjectCodeBox)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.MapperCodeBox)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.ExtensionsCodeBox)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.EnumsCodeBox)).BeginInit();
            this.menuStrip1.SuspendLayout();
            this.SuspendLayout();
            // 
            // CodeGeneratorTabControl
            // 
            this.CodeGeneratorTabControl.Controls.Add(this.ConfigurationTabPage);
            this.CodeGeneratorTabControl.Controls.Add(this.BusinessObjectTabPage);
            this.CodeGeneratorTabControl.Controls.Add(this.MapperTabPage);
            this.CodeGeneratorTabControl.Controls.Add(this.ExtensionMethods);
            this.CodeGeneratorTabControl.Controls.Add(this.EnumsTabPage);
            this.CodeGeneratorTabControl.Dock = System.Windows.Forms.DockStyle.Fill;
            this.CodeGeneratorTabControl.Location = new System.Drawing.Point(0, 24);
            this.CodeGeneratorTabControl.Name = "CodeGeneratorTabControl";
            this.CodeGeneratorTabControl.SelectedIndex = 0;
            this.CodeGeneratorTabControl.Size = new System.Drawing.Size(1071, 540);
            this.CodeGeneratorTabControl.TabIndex = 0;
            // 
            // ConfigurationTabPage
            // 
            this.ConfigurationTabPage.Controls.Add(this.toolStrip1);
            this.ConfigurationTabPage.Controls.Add(this.ConfigurationDataGridView);
            this.ConfigurationTabPage.Location = new System.Drawing.Point(4, 22);
            this.ConfigurationTabPage.Name = "ConfigurationTabPage";
            this.ConfigurationTabPage.Padding = new System.Windows.Forms.Padding(3);
            this.ConfigurationTabPage.Size = new System.Drawing.Size(1063, 514);
            this.ConfigurationTabPage.TabIndex = 0;
            this.ConfigurationTabPage.Text = "Configuration";
            this.ConfigurationTabPage.UseVisualStyleBackColor = true;
            // 
            // ConfigurationDataGridView
            // 
            this.ConfigurationDataGridView.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.ConfigurationDataGridView.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill;
            this.ConfigurationDataGridView.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.ConfigurationDataGridView.Location = new System.Drawing.Point(0, 31);
            this.ConfigurationDataGridView.Name = "ConfigurationDataGridView";
            this.ConfigurationDataGridView.Size = new System.Drawing.Size(1063, 483);
            this.ConfigurationDataGridView.TabIndex = 0;
            // 
            // BusinessObjectTabPage
            // 
            this.BusinessObjectTabPage.Controls.Add(this.BusinessObjectCodeBox);
            this.BusinessObjectTabPage.Location = new System.Drawing.Point(4, 22);
            this.BusinessObjectTabPage.Name = "BusinessObjectTabPage";
            this.BusinessObjectTabPage.Padding = new System.Windows.Forms.Padding(3);
            this.BusinessObjectTabPage.Size = new System.Drawing.Size(1063, 514);
            this.BusinessObjectTabPage.TabIndex = 1;
            this.BusinessObjectTabPage.Text = "Business Object";
            this.BusinessObjectTabPage.UseVisualStyleBackColor = true;
            // 
            // MapperTabPage
            // 
            this.MapperTabPage.Controls.Add(this.MapperCodeBox);
            this.MapperTabPage.Location = new System.Drawing.Point(4, 22);
            this.MapperTabPage.Name = "MapperTabPage";
            this.MapperTabPage.Padding = new System.Windows.Forms.Padding(3);
            this.MapperTabPage.Size = new System.Drawing.Size(1038, 513);
            this.MapperTabPage.TabIndex = 2;
            this.MapperTabPage.Text = "Mapper";
            this.MapperTabPage.UseVisualStyleBackColor = true;
            // 
            // ExtensionMethods
            // 
            this.ExtensionMethods.Controls.Add(this.ExtensionsCodeBox);
            this.ExtensionMethods.Location = new System.Drawing.Point(4, 22);
            this.ExtensionMethods.Name = "ExtensionMethods";
            this.ExtensionMethods.Padding = new System.Windows.Forms.Padding(3);
            this.ExtensionMethods.Size = new System.Drawing.Size(1038, 513);
            this.ExtensionMethods.TabIndex = 3;
            this.ExtensionMethods.Text = "Extensions";
            this.ExtensionMethods.UseVisualStyleBackColor = true;
            // 
            // EnumsTabPage
            // 
            this.EnumsTabPage.Controls.Add(this.EnumsCodeBox);
            this.EnumsTabPage.Location = new System.Drawing.Point(4, 22);
            this.EnumsTabPage.Name = "EnumsTabPage";
            this.EnumsTabPage.Padding = new System.Windows.Forms.Padding(3);
            this.EnumsTabPage.Size = new System.Drawing.Size(1063, 514);
            this.EnumsTabPage.TabIndex = 4;
            this.EnumsTabPage.Text = "Enums";
            this.EnumsTabPage.UseVisualStyleBackColor = true;
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
            this.BusinessObjectCodeBox.AutoScrollMinSize = new System.Drawing.Size(179, 14);
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
            this.BusinessObjectCodeBox.Size = new System.Drawing.Size(1057, 508);
            this.BusinessObjectCodeBox.TabIndex = 0;
            this.BusinessObjectCodeBox.Text = "fastColoredTextBox1";
            this.BusinessObjectCodeBox.Zoom = 100;
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
            this.MapperCodeBox.AutoScrollMinSize = new System.Drawing.Size(179, 14);
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
            this.MapperCodeBox.Size = new System.Drawing.Size(1032, 507);
            this.MapperCodeBox.TabIndex = 0;
            this.MapperCodeBox.Text = "fastColoredTextBox1";
            this.MapperCodeBox.Zoom = 100;
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
            this.ExtensionsCodeBox.AutoScrollMinSize = new System.Drawing.Size(179, 14);
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
            this.ExtensionsCodeBox.Size = new System.Drawing.Size(1032, 507);
            this.ExtensionsCodeBox.TabIndex = 0;
            this.ExtensionsCodeBox.Text = "fastColoredTextBox1";
            this.ExtensionsCodeBox.Zoom = 100;
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
            this.EnumsCodeBox.AutoScrollMinSize = new System.Drawing.Size(179, 14);
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
            this.EnumsCodeBox.Size = new System.Drawing.Size(1057, 508);
            this.EnumsCodeBox.TabIndex = 0;
            this.EnumsCodeBox.Text = "fastColoredTextBox1";
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
            // toolStrip1
            // 
            this.toolStrip1.Location = new System.Drawing.Point(3, 3);
            this.toolStrip1.Name = "toolStrip1";
            this.toolStrip1.Size = new System.Drawing.Size(1057, 25);
            this.toolStrip1.TabIndex = 1;
            this.toolStrip1.Text = "toolStrip1";
            // 
            // CodeGeneratorMainWindow
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1071, 564);
            this.Controls.Add(this.CodeGeneratorTabControl);
            this.Controls.Add(this.menuStrip1);
            this.MainMenuStrip = this.menuStrip1;
            this.Name = "CodeGeneratorMainWindow";
            this.Text = "Canal Code Generator";
            this.CodeGeneratorTabControl.ResumeLayout(false);
            this.ConfigurationTabPage.ResumeLayout(false);
            this.ConfigurationTabPage.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.ConfigurationDataGridView)).EndInit();
            this.BusinessObjectTabPage.ResumeLayout(false);
            this.MapperTabPage.ResumeLayout(false);
            this.ExtensionMethods.ResumeLayout(false);
            this.EnumsTabPage.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.BusinessObjectCodeBox)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.MapperCodeBox)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.ExtensionsCodeBox)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.EnumsCodeBox)).EndInit();
            this.menuStrip1.ResumeLayout(false);
            this.menuStrip1.PerformLayout();
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
        private System.Windows.Forms.ToolStrip toolStrip1;
    }
}

