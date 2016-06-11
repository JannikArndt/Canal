namespace Canal.UserControls
{
    partial class WordInfo
    {
        /// <summary> 
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;


        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.wordInfoTreeView = new System.Windows.Forms.TreeView();
            this.infoGroupBox = new System.Windows.Forms.GroupBox();
            this.gotoFileButton = new System.Windows.Forms.Button();
            this.infoGroupBox.SuspendLayout();
            this.SuspendLayout();
            // 
            // wordInfoTreeView
            // 
            this.wordInfoTreeView.BackColor = System.Drawing.SystemColors.Control;
            this.wordInfoTreeView.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.wordInfoTreeView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.wordInfoTreeView.Font = new System.Drawing.Font("Consolas", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.wordInfoTreeView.ItemHeight = 22;
            this.wordInfoTreeView.Location = new System.Drawing.Point(3, 16);
            this.wordInfoTreeView.Name = "wordInfoTreeView";
            this.wordInfoTreeView.Size = new System.Drawing.Size(288, 454);
            this.wordInfoTreeView.TabIndex = 1;
            // 
            // infoGroupBox
            // 
            this.infoGroupBox.Controls.Add(this.gotoFileButton);
            this.infoGroupBox.Controls.Add(this.wordInfoTreeView);
            this.infoGroupBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.infoGroupBox.Location = new System.Drawing.Point(0, 0);
            this.infoGroupBox.Name = "infoGroupBox";
            this.infoGroupBox.Size = new System.Drawing.Size(294, 473);
            this.infoGroupBox.TabIndex = 2;
            this.infoGroupBox.TabStop = false;
            // 
            // gotoFileButton
            // 
            this.gotoFileButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.gotoFileButton.Location = new System.Drawing.Point(178, 19);
            this.gotoFileButton.Name = "gotoFileButton";
            this.gotoFileButton.Size = new System.Drawing.Size(110, 23);
            this.gotoFileButton.TabIndex = 2;
            this.gotoFileButton.Text = "Open Copybook";
            this.gotoFileButton.UseVisualStyleBackColor = true;
            this.gotoFileButton.Visible = false;
            // 
            // WordInfo
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.infoGroupBox);
            this.Name = "WordInfo";
            this.Size = new System.Drawing.Size(294, 473);
            this.infoGroupBox.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion
        private System.Windows.Forms.TreeView wordInfoTreeView;
        private System.Windows.Forms.GroupBox infoGroupBox;
        private System.Windows.Forms.Button gotoFileButton;
    }
}
