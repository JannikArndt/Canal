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
            this.VariableInfoTreeView = new Canal.UserControls.WordInfoViews.VariableTreeView();
            this.SuspendLayout();
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
            this.Controls.Add(this.VariableInfoTreeView);
            this.Name = "VariableInfo";
            this.Size = new System.Drawing.Size(359, 515);
            this.ResumeLayout(false);

        }

        #endregion

        private VariableTreeView VariableInfoTreeView;
    }
}
