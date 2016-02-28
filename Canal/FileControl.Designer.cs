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
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(FileControl));
            this.codeBox1 = new Canal.CodeBox();
            this.treeView1 = new System.Windows.Forms.TreeView();
            ((System.ComponentModel.ISupportInitialize)(this.codeBox1)).BeginInit();
            this.SuspendLayout();
            // 
            // codeBox1
            // 
            this.codeBox1.AutoCompleteBracketsList = new char[] {
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
            this.codeBox1.AutoScrollMinSize = new System.Drawing.Size(111, 18);
            this.codeBox1.BackBrush = null;
            this.codeBox1.CharHeight = 18;
            this.codeBox1.CharWidth = 10;
            this.codeBox1.Cursor = System.Windows.Forms.Cursors.IBeam;
            this.codeBox1.DisabledColor = System.Drawing.Color.FromArgb(((int)(((byte)(100)))), ((int)(((byte)(180)))), ((int)(((byte)(180)))), ((int)(((byte)(180)))));
            this.codeBox1.IsReplaceMode = false;
            this.codeBox1.Location = new System.Drawing.Point(3, 3);
            this.codeBox1.Name = "codeBox1";
            this.codeBox1.Paddings = new System.Windows.Forms.Padding(0);
            this.codeBox1.SelectionColor = System.Drawing.Color.FromArgb(((int)(((byte)(60)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(255)))));
            this.codeBox1.ServiceColors = ((FastColoredTextBoxNS.ServiceColors)(resources.GetObject("codeBox1.ServiceColors")));
            this.codeBox1.Size = new System.Drawing.Size(780, 712);
            this.codeBox1.TabIndex = 0;
            this.codeBox1.Text = "codeBox1";
            this.codeBox1.Zoom = 100;
            // 
            // treeView1
            // 
            this.treeView1.Location = new System.Drawing.Point(790, 4);
            this.treeView1.Name = "treeView1";
            this.treeView1.Size = new System.Drawing.Size(310, 714);
            this.treeView1.TabIndex = 1;
            // 
            // FileControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 16F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.treeView1);
            this.Controls.Add(this.codeBox1);
            this.Name = "FileControl";
            this.Size = new System.Drawing.Size(1103, 718);
            ((System.ComponentModel.ISupportInitialize)(this.codeBox1)).EndInit();
            this.ResumeLayout(false);

        }

        #endregion

        private CodeBox codeBox1;
        private System.Windows.Forms.TreeView treeView1;
    }
}
