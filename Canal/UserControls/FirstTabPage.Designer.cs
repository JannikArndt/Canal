namespace Canal.UserControls
{
    partial class FirstTabPage
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
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.openAllButton = new System.Windows.Forms.Button();
            this.recentFilesListView = new System.Windows.Forms.ListView();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.changeLogTextBox = new System.Windows.Forms.TextBox();
            this.groupBox1.SuspendLayout();
            this.groupBox2.SuspendLayout();
            this.SuspendLayout();
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.openAllButton);
            this.groupBox1.Controls.Add(this.recentFilesListView);
            this.groupBox1.Location = new System.Drawing.Point(21, 31);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(473, 377);
            this.groupBox1.TabIndex = 3;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Recent Files";
            // 
            // openAllButton
            // 
            this.openAllButton.Location = new System.Drawing.Point(7, 347);
            this.openAllButton.Name = "openAllButton";
            this.openAllButton.Size = new System.Drawing.Size(75, 23);
            this.openAllButton.TabIndex = 2;
            this.openAllButton.Text = "Open All";
            this.openAllButton.UseVisualStyleBackColor = true;
            this.openAllButton.Click += new System.EventHandler(this.OpenAllRecentFiles);
            // 
            // recentFilesListView
            // 
            this.recentFilesListView.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.recentFilesListView.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.recentFilesListView.Location = new System.Drawing.Point(6, 19);
            this.recentFilesListView.Name = "recentFilesListView";
            this.recentFilesListView.Size = new System.Drawing.Size(461, 321);
            this.recentFilesListView.TabIndex = 1;
            this.recentFilesListView.UseCompatibleStateImageBehavior = false;
            this.recentFilesListView.View = System.Windows.Forms.View.List;
            this.recentFilesListView.DoubleClick += new System.EventHandler(this.recentFilesListView_DoubleClick);
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.changeLogTextBox);
            this.groupBox2.Location = new System.Drawing.Point(549, 31);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(508, 377);
            this.groupBox2.TabIndex = 4;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "New in this Version";
            // 
            // changeLogTextBox
            // 
            this.changeLogTextBox.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.changeLogTextBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.changeLogTextBox.Location = new System.Drawing.Point(3, 16);
            this.changeLogTextBox.Multiline = true;
            this.changeLogTextBox.Name = "changeLogTextBox";
            this.changeLogTextBox.ReadOnly = true;
            this.changeLogTextBox.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.changeLogTextBox.Size = new System.Drawing.Size(502, 358);
            this.changeLogTextBox.TabIndex = 0;
            // 
            // FirstTabPage
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.groupBox2);
            this.Controls.Add(this.groupBox1);
            this.Name = "FirstTabPage";
            this.Size = new System.Drawing.Size(1148, 537);
            this.groupBox1.ResumeLayout(false);
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.Button openAllButton;
        private System.Windows.Forms.ListView recentFilesListView;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.TextBox changeLogTextBox;
    }
}
