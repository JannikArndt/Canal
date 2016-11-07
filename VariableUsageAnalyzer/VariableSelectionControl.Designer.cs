namespace VariableUsageAnalyzer
{
    partial class VariableSelectionControl
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
            this.variableTreeView1 = new VariableUsageAnalyzer.VariableTreeView();
            this.SuspendLayout();
            // 
            // variableTreeView1
            // 
            this.variableTreeView1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.variableTreeView1.Location = new System.Drawing.Point(0, 0);
            this.variableTreeView1.Name = "variableTreeView1";
            this.variableTreeView1.Size = new System.Drawing.Size(520, 658);
            this.variableTreeView1.TabIndex = 0;
            // 
            // VariableSelectionControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.variableTreeView1);
            this.Name = "VariableSelectionControl";
            this.Size = new System.Drawing.Size(520, 658);
            this.ResumeLayout(false);

        }

        #endregion

        private VariableTreeView variableTreeView1;
    }
}
