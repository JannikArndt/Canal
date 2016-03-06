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
            this.treeView = new System.Windows.Forms.TreeView();
            this.searchBox = new System.Windows.Forms.TextBox();
            this.searchWithRegEx = new System.Windows.Forms.CheckBox();
            this.codeBox = new Canal.CodeBox();
            this.ResolveCopysButton = new System.Windows.Forms.Button();
            ((System.ComponentModel.ISupportInitialize)(this.codeBox)).BeginInit();
            this.SuspendLayout();
            // 
            // treeView
            // 
            this.treeView.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.treeView.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.treeView.Font = new System.Drawing.Font("Consolas", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.treeView.ItemHeight = 22;
            this.treeView.Location = new System.Drawing.Point(591, 28);
            this.treeView.Margin = new System.Windows.Forms.Padding(2);
            this.treeView.Name = "treeView";
            this.treeView.Size = new System.Drawing.Size(234, 553);
            this.treeView.TabIndex = 1;
            this.treeView.AfterSelect += new System.Windows.Forms.TreeViewEventHandler(this.treeView_AfterSelect);
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
            this.codeBox.AutoScrollMinSize = new System.Drawing.Size(25, 15);
            this.codeBox.BackBrush = null;
            this.codeBox.CharHeight = 15;
            this.codeBox.CharWidth = 7;
            this.codeBox.CobolFile = null;
            this.codeBox.Cursor = System.Windows.Forms.Cursors.IBeam;
            this.codeBox.DisabledColor = System.Drawing.Color.FromArgb(((int)(((byte)(100)))), ((int)(((byte)(180)))), ((int)(((byte)(180)))), ((int)(((byte)(180)))));
            this.codeBox.Font = new System.Drawing.Font("Consolas", 9.75F);
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
            // ResolveCopysButton
            // 
            this.ResolveCopysButton.Location = new System.Drawing.Point(241, 0);
            this.ResolveCopysButton.Name = "ResolveCopysButton";
            this.ResolveCopysButton.Size = new System.Drawing.Size(92, 23);
            this.ResolveCopysButton.TabIndex = 4;
            this.ResolveCopysButton.Text = "Resolve COPYs";
            this.ResolveCopysButton.UseVisualStyleBackColor = true;
            this.ResolveCopysButton.Click += new System.EventHandler(this.ResolveCopysButton_Click);
            // 
            // FileControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.ResolveCopysButton);
            this.Controls.Add(this.searchWithRegEx);
            this.Controls.Add(this.searchBox);
            this.Controls.Add(this.treeView);
            this.Controls.Add(this.codeBox);
            this.Margin = new System.Windows.Forms.Padding(2);
            this.Name = "FileControl";
            this.Size = new System.Drawing.Size(827, 583);
            ((System.ComponentModel.ISupportInitialize)(this.codeBox)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion
        private System.Windows.Forms.TreeView treeView;
        private System.Windows.Forms.TextBox searchBox;
        private CodeBox codeBox;
        private System.Windows.Forms.CheckBox searchWithRegEx;
        private System.Windows.Forms.Button ResolveCopysButton;
    }
}
