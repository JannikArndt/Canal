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
            this.VariableInfoTreeView = new Canal.UserControls.WordInfoViews.VariableTreeView();
            this.SuspendLayout();
            // 
            // gotoFileButton
            // 
            this.gotoFileButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.gotoFileButton.Location = new System.Drawing.Point(243, 19);
            this.gotoFileButton.Name = "gotoFileButton";
            this.gotoFileButton.Size = new System.Drawing.Size(110, 23);
            this.gotoFileButton.TabIndex = 4;
            this.gotoFileButton.Text = "Open Copybook";
            this.gotoFileButton.UseVisualStyleBackColor = true;
            this.gotoFileButton.Visible = false;
            // 
            // VariableInfoTreeView
            // 
            this.VariableInfoTreeView.BackColor = System.Drawing.SystemColors.Control;
            this.VariableInfoTreeView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.VariableInfoTreeView.Location = new System.Drawing.Point(0, 0);
            this.VariableInfoTreeView.Name = "VariableInfoTreeView";
            this.VariableInfoTreeView.Size = new System.Drawing.Size(359, 515);
            this.VariableInfoTreeView.TabIndex = 3;
            // 
            // VariableInfo
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.Controls.Add(this.gotoFileButton);
            this.Controls.Add(this.VariableInfoTreeView);
            this.Name = "VariableInfo";
            this.Size = new System.Drawing.Size(359, 515);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button gotoFileButton;
        private VariableTreeView VariableInfoTreeView;
    }
}
