namespace Canal.UserControls
{
    partial class WordInfo
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
            this.headingLabel = new System.Windows.Forms.Label();
            this.variableTreeView = new System.Windows.Forms.TreeView();
            this.SuspendLayout();
            // 
            // headingLabel
            // 
            this.headingLabel.AutoSize = true;
            this.headingLabel.Font = new System.Drawing.Font("Consolas", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.headingLabel.Location = new System.Drawing.Point(4, 4);
            this.headingLabel.Name = "headingLabel";
            this.headingLabel.Size = new System.Drawing.Size(63, 19);
            this.headingLabel.TabIndex = 0;
            this.headingLabel.Text = "label1";
            // 
            // variableTreeView
            // 
            this.variableTreeView.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.variableTreeView.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.variableTreeView.Location = new System.Drawing.Point(0, 35);
            this.variableTreeView.Name = "variableTreeView";
            this.variableTreeView.Size = new System.Drawing.Size(294, 435);
            this.variableTreeView.TabIndex = 1;
            // 
            // WordInfo
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.variableTreeView);
            this.Controls.Add(this.headingLabel);
            this.Name = "WordInfo";
            this.Size = new System.Drawing.Size(294, 473);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label headingLabel;
        private System.Windows.Forms.TreeView variableTreeView;
    }
}
