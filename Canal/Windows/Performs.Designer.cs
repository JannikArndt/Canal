namespace Canal.Windows
{
    partial class Performs
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Performs));
            this.performTextBox = new FastColoredTextBoxNS.FastColoredTextBox();
            ((System.ComponentModel.ISupportInitialize)(this.performTextBox)).BeginInit();
            this.SuspendLayout();
            // 
            // performTextBox
            // 
            this.performTextBox.AutoCompleteBracketsList = new char[] {
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
            this.performTextBox.AutoScrollMinSize = new System.Drawing.Size(179, 14);
            this.performTextBox.BackBrush = null;
            this.performTextBox.CharHeight = 14;
            this.performTextBox.CharWidth = 8;
            this.performTextBox.Cursor = System.Windows.Forms.Cursors.IBeam;
            this.performTextBox.DisabledColor = System.Drawing.Color.FromArgb(((int)(((byte)(100)))), ((int)(((byte)(180)))), ((int)(((byte)(180)))), ((int)(((byte)(180)))));
            this.performTextBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.performTextBox.IsReplaceMode = false;
            this.performTextBox.Location = new System.Drawing.Point(0, 0);
            this.performTextBox.Name = "performTextBox";
            this.performTextBox.Paddings = new System.Windows.Forms.Padding(0);
            this.performTextBox.SelectionColor = System.Drawing.Color.FromArgb(((int)(((byte)(60)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(255)))));
            this.performTextBox.ServiceColors = ((FastColoredTextBoxNS.ServiceColors)(resources.GetObject("performTextBox.ServiceColors")));
            this.performTextBox.Size = new System.Drawing.Size(653, 519);
            this.performTextBox.TabIndex = 0;
            this.performTextBox.Text = "fastColoredTextBox1";
            this.performTextBox.Zoom = 100;
            // 
            // Performs
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(653, 519);
            this.Controls.Add(this.performTextBox);
            this.Name = "Performs";
            this.Text = "Performs";
            ((System.ComponentModel.ISupportInitialize)(this.performTextBox)).EndInit();
            this.ResumeLayout(false);

        }

        #endregion

        private FastColoredTextBoxNS.FastColoredTextBox performTextBox;

    }
}