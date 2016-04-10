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
            this.variableTreeView = new System.Windows.Forms.TreeView();
            this.infoGroupBox = new System.Windows.Forms.GroupBox();
            this.infoGroupBox.SuspendLayout();
            this.SuspendLayout();
            // 
            // variableTreeView
            // 
            this.variableTreeView.BackColor = System.Drawing.SystemColors.Control;
            this.variableTreeView.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.variableTreeView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.variableTreeView.Font = new System.Drawing.Font("Consolas", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.variableTreeView.ItemHeight = 22;
            this.variableTreeView.Location = new System.Drawing.Point(3, 16);
            this.variableTreeView.Name = "variableTreeView";
            this.variableTreeView.Size = new System.Drawing.Size(288, 454);
            this.variableTreeView.TabIndex = 1;
            // 
            // infoGroupBox
            // 
            this.infoGroupBox.Controls.Add(this.variableTreeView);
            this.infoGroupBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.infoGroupBox.Location = new System.Drawing.Point(0, 0);
            this.infoGroupBox.Name = "infoGroupBox";
            this.infoGroupBox.Size = new System.Drawing.Size(294, 473);
            this.infoGroupBox.TabIndex = 2;
            this.infoGroupBox.TabStop = false;
            this.infoGroupBox.Text = "groupBox1";
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
        private System.Windows.Forms.TreeView variableTreeView;
        private System.Windows.Forms.GroupBox infoGroupBox;
    }
}
