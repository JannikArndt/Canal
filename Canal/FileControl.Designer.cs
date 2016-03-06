namespace Canal
{
    partial class FileControl
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
        /// <param name="file"></param>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(FileControl));
            this.treeView1 = new System.Windows.Forms.TreeView();
            this.searchBox = new System.Windows.Forms.TextBox();
            this.codeBox = new Canal.CodeBox();
            this.searchWithRegEx = new System.Windows.Forms.CheckBox();
            ((System.ComponentModel.ISupportInitialize)(this.codeBox)).BeginInit();
            this.SuspendLayout();
            // 
            // treeView1
            // 
            this.treeView1.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.treeView1.Location = new System.Drawing.Point(591, 28);
            this.treeView1.Margin = new System.Windows.Forms.Padding(2);
            this.treeView1.Name = "treeView1";
            this.treeView1.Size = new System.Drawing.Size(234, 553);
            this.treeView1.TabIndex = 1;
            // 
            // searchBox
            // 
            this.searchBox.Location = new System.Drawing.Point(3, 3);
            this.searchBox.Name = "searchBox";
            this.searchBox.Size = new System.Drawing.Size(167, 20);
            this.searchBox.TabIndex = 2;
            this.searchBox.Text = "Search...";
            this.searchBox.WordWrap = false;
            this.searchBox.TextChanged += new System.EventHandler(this.seachBox_TextChanged);
            this.searchBox.Enter += new System.EventHandler(this.searchBox_Enter);
            this.searchBox.KeyDown += new System.Windows.Forms.KeyEventHandler(this.searchBox_KeyDown);
            this.searchBox.Leave += new System.EventHandler(this.searchBox_Leave);
            // 
            // codeBox
            // 
            this.codeBox.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.codeBox.AutoCompleteBracketsList = new char[] {
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
            this.codeBox.AutoScrollMinSize = new System.Drawing.Size(27, 14);
            this.codeBox.BackBrush = null;
            this.codeBox.CharHeight = 14;
            this.codeBox.CharWidth = 8;
            this.codeBox.CobolFile = null;
            this.codeBox.Cursor = System.Windows.Forms.Cursors.IBeam;
            this.codeBox.DisabledColor = System.Drawing.Color.FromArgb(((int)(((byte)(100)))), ((int)(((byte)(180)))), ((int)(((byte)(180)))), ((int)(((byte)(180)))));
            this.codeBox.IsReplaceMode = false;
            this.codeBox.Location = new System.Drawing.Point(3, 28);
            this.codeBox.Margin = new System.Windows.Forms.Padding(2);
            this.codeBox.Name = "codeBox";
            this.codeBox.Paddings = new System.Windows.Forms.Padding(0);
            this.codeBox.SelectionColor = System.Drawing.Color.FromArgb(((int)(((byte)(60)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(255)))));
            this.codeBox.ServiceColors = ((FastColoredTextBoxNS.ServiceColors)(resources.GetObject("codeBox.ServiceColors")));
            this.codeBox.Size = new System.Drawing.Size(585, 553);
            this.codeBox.TabIndex = 0;
            this.codeBox.Zoom = 100;
            // 
            // searchWithRegEx
            // 
            this.searchWithRegEx.AutoSize = true;
            this.searchWithRegEx.Location = new System.Drawing.Point(177, 5);
            this.searchWithRegEx.Name = "searchWithRegEx";
            this.searchWithRegEx.Size = new System.Drawing.Size(58, 17);
            this.searchWithRegEx.TabIndex = 3;
            this.searchWithRegEx.Text = "RegEx";
            this.searchWithRegEx.UseVisualStyleBackColor = true;
            // 
            // FileControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.searchWithRegEx);
            this.Controls.Add(this.searchBox);
            this.Controls.Add(this.treeView1);
            this.Controls.Add(this.codeBox);
            this.Margin = new System.Windows.Forms.Padding(2);
            this.Name = "FileControl";
            this.Size = new System.Drawing.Size(827, 583);
            ((System.ComponentModel.ISupportInitialize)(this.codeBox)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion
        private System.Windows.Forms.TreeView treeView1;
        private System.Windows.Forms.TextBox searchBox;
        private CodeBox codeBox;
        private System.Windows.Forms.CheckBox searchWithRegEx;
    }
}
