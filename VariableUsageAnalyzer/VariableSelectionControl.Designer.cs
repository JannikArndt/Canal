namespace VariableUsageAnalyzer
{
    sealed partial class VariableSelectionControl
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(VariableSelectionControl));
            this.splitContainer1 = new System.Windows.Forms.SplitContainer();
            this.label1 = new System.Windows.Forms.Label();
            this.panel1 = new System.Windows.Forms.Panel();
            this.includeDirectAndIndirectChildVariablesCheckBox = new System.Windows.Forms.CheckBox();
            this.includeRedefinesCheckBox = new System.Windows.Forms.CheckBox();
            this.VariableSelectionTreeView = new VariableUsageAnalyzer.VariableTreeView();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer1)).BeginInit();
            this.splitContainer1.Panel1.SuspendLayout();
            this.splitContainer1.Panel2.SuspendLayout();
            this.splitContainer1.SuspendLayout();
            this.panel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // splitContainer1
            // 
            this.splitContainer1.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.splitContainer1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer1.FixedPanel = System.Windows.Forms.FixedPanel.Panel1;
            this.splitContainer1.Location = new System.Drawing.Point(0, 0);
            this.splitContainer1.Name = "splitContainer1";
            this.splitContainer1.Orientation = System.Windows.Forms.Orientation.Horizontal;
            // 
            // splitContainer1.Panel1
            // 
            this.splitContainer1.Panel1.Controls.Add(this.panel1);
            this.splitContainer1.Panel1.Controls.Add(this.label1);
            this.splitContainer1.Panel1.Padding = new System.Windows.Forms.Padding(3);
            // 
            // splitContainer1.Panel2
            // 
            this.splitContainer1.Panel2.Controls.Add(this.VariableSelectionTreeView);
            this.splitContainer1.Size = new System.Drawing.Size(520, 658);
            this.splitContainer1.SplitterDistance = 110;
            this.splitContainer1.TabIndex = 0;
            // 
            // label1
            // 
            this.label1.Dock = System.Windows.Forms.DockStyle.Top;
            this.label1.ForeColor = System.Drawing.SystemColors.GrayText;
            this.label1.Location = new System.Drawing.Point(3, 3);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(512, 78);
            this.label1.TabIndex = 2;
            this.label1.Text = resources.GetString("label1.Text");
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.includeRedefinesCheckBox);
            this.panel1.Controls.Add(this.includeDirectAndIndirectChildVariablesCheckBox);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.panel1.Location = new System.Drawing.Point(3, 54);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(512, 51);
            this.panel1.TabIndex = 3;
            // 
            // includeDirectAndIndirectChildVariablesCheckBox
            // 
            this.includeDirectAndIndirectChildVariablesCheckBox.AutoSize = true;
            this.includeDirectAndIndirectChildVariablesCheckBox.Location = new System.Drawing.Point(3, 10);
            this.includeDirectAndIndirectChildVariablesCheckBox.Name = "includeDirectAndIndirectChildVariablesCheckBox";
            this.includeDirectAndIndirectChildVariablesCheckBox.Size = new System.Drawing.Size(218, 17);
            this.includeDirectAndIndirectChildVariablesCheckBox.TabIndex = 0;
            this.includeDirectAndIndirectChildVariablesCheckBox.Text = "Include direct and indirect child variables";
            this.includeDirectAndIndirectChildVariablesCheckBox.UseVisualStyleBackColor = true;
            // 
            // includeRedefinesCheckBox
            // 
            this.includeRedefinesCheckBox.AutoSize = true;
            this.includeRedefinesCheckBox.Location = new System.Drawing.Point(3, 30);
            this.includeRedefinesCheckBox.Name = "includeRedefinesCheckBox";
            this.includeRedefinesCheckBox.Size = new System.Drawing.Size(107, 17);
            this.includeRedefinesCheckBox.TabIndex = 1;
            this.includeRedefinesCheckBox.Text = "Include redefines";
            this.includeRedefinesCheckBox.UseVisualStyleBackColor = true;
            // 
            // VariableSelectionTreeView
            // 
            this.VariableSelectionTreeView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.VariableSelectionTreeView.Location = new System.Drawing.Point(0, 0);
            this.VariableSelectionTreeView.Name = "VariableSelectionTreeView";
            this.VariableSelectionTreeView.Size = new System.Drawing.Size(518, 542);
            this.VariableSelectionTreeView.TabIndex = 1;
            // 
            // VariableSelectionControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.splitContainer1);
            this.Margin = new System.Windows.Forms.Padding(0);
            this.Name = "VariableSelectionControl";
            this.Size = new System.Drawing.Size(520, 658);
            this.splitContainer1.Panel1.ResumeLayout(false);
            this.splitContainer1.Panel2.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer1)).EndInit();
            this.splitContainer1.ResumeLayout(false);
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.SplitContainer splitContainer1;
        public VariableTreeView VariableSelectionTreeView;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.CheckBox includeRedefinesCheckBox;
        private System.Windows.Forms.CheckBox includeDirectAndIndirectChildVariablesCheckBox;
    }
}
