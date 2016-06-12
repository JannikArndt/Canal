namespace Canal.UserControls.WordInfoViews
{
    partial class VariableInfo
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
            this.gotoFileButton = new System.Windows.Forms.Button();
            this.infoGroupBox = new System.Windows.Forms.GroupBox();
            this.VariableInfoTreeView = new System.Windows.Forms.TreeView();
            this.infoGroupBox.SuspendLayout();
            this.SuspendLayout();
            // 
            // gotoFileButton
            // 
            this.gotoFileButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.gotoFileButton.Location = new System.Drawing.Point(245, 19);
            this.gotoFileButton.Name = "gotoFileButton";
            this.gotoFileButton.Size = new System.Drawing.Size(110, 23);
            this.gotoFileButton.TabIndex = 2;
            this.gotoFileButton.Text = "Open Copybook";
            this.gotoFileButton.UseVisualStyleBackColor = true;
            this.gotoFileButton.Visible = false;
            // 
            // infoGroupBox
            // 
            this.infoGroupBox.Controls.Add(this.gotoFileButton);
            this.infoGroupBox.Controls.Add(this.VariableInfoTreeView);
            this.infoGroupBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.infoGroupBox.Location = new System.Drawing.Point(0, 0);
            this.infoGroupBox.Name = "infoGroupBox";
            this.infoGroupBox.Size = new System.Drawing.Size(361, 517);
            this.infoGroupBox.TabIndex = 3;
            this.infoGroupBox.TabStop = false;
            // 
            // VariableInfoTreeView
            // 
            this.VariableInfoTreeView.BackColor = System.Drawing.SystemColors.Control;
            this.VariableInfoTreeView.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.VariableInfoTreeView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.VariableInfoTreeView.Font = new System.Drawing.Font("Consolas", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.VariableInfoTreeView.ItemHeight = 22;
            this.VariableInfoTreeView.Location = new System.Drawing.Point(3, 16);
            this.VariableInfoTreeView.Name = "VariableInfoTreeView";
            this.VariableInfoTreeView.Size = new System.Drawing.Size(355, 498);
            this.VariableInfoTreeView.TabIndex = 1;
            // 
            // VariableInfo
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.infoGroupBox);
            this.Name = "VariableInfo";
            this.Size = new System.Drawing.Size(361, 517);
            this.infoGroupBox.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button gotoFileButton;
        private System.Windows.Forms.GroupBox infoGroupBox;
        private System.Windows.Forms.TreeView VariableInfoTreeView;
    }
}
