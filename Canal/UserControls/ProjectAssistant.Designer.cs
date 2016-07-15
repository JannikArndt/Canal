namespace Canal.UserControls
{
    partial class ProjectAssistant
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(ProjectAssistant));
            this.tableLayoutPanel1 = new System.Windows.Forms.TableLayoutPanel();
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.NameTextBox = new System.Windows.Forms.TextBox();
            this.FolderTextBox = new System.Windows.Forms.TextBox();
            this.FileTypesCheckedListBox = new System.Windows.Forms.CheckedListBox();
            this.ProgressBar = new System.Windows.Forms.ProgressBar();
            this.ProgressLabel = new System.Windows.Forms.Label();
            this.StartAnalysisButton = new System.Windows.Forms.Button();
            this.CancelButton1 = new System.Windows.Forms.Button();
            this.pictureBox1 = new System.Windows.Forms.PictureBox();
            this.folderBrowserDialog = new System.Windows.Forms.FolderBrowserDialog();
            this.tableLayoutPanel1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).BeginInit();
            this.SuspendLayout();
            // 
            // tableLayoutPanel1
            // 
            this.tableLayoutPanel1.ColumnCount = 6;
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 100F));
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 21F));
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 170F));
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 138F));
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 148F));
            this.tableLayoutPanel1.Controls.Add(this.label1, 1, 0);
            this.tableLayoutPanel1.Controls.Add(this.label2, 1, 1);
            this.tableLayoutPanel1.Controls.Add(this.label3, 1, 2);
            this.tableLayoutPanel1.Controls.Add(this.label4, 1, 3);
            this.tableLayoutPanel1.Controls.Add(this.NameTextBox, 3, 1);
            this.tableLayoutPanel1.Controls.Add(this.FolderTextBox, 3, 2);
            this.tableLayoutPanel1.Controls.Add(this.FileTypesCheckedListBox, 3, 3);
            this.tableLayoutPanel1.Controls.Add(this.ProgressBar, 1, 4);
            this.tableLayoutPanel1.Controls.Add(this.ProgressLabel, 3, 4);
            this.tableLayoutPanel1.Controls.Add(this.StartAnalysisButton, 4, 4);
            this.tableLayoutPanel1.Controls.Add(this.CancelButton1, 5, 4);
            this.tableLayoutPanel1.Controls.Add(this.pictureBox1, 5, 2);
            this.tableLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tableLayoutPanel1.Location = new System.Drawing.Point(0, 0);
            this.tableLayoutPanel1.Name = "tableLayoutPanel1";
            this.tableLayoutPanel1.RowCount = 5;
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 60F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 40F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 40F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 84F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 46F));
            this.tableLayoutPanel1.Size = new System.Drawing.Size(597, 272);
            this.tableLayoutPanel1.TabIndex = 0;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.tableLayoutPanel1.SetColumnSpan(this.label1, 3);
            this.label1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.label1.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label1.Location = new System.Drawing.Point(23, 0);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(285, 60);
            this.label1.TabIndex = 0;
            this.label1.Text = "New Project";
            this.label1.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Dock = System.Windows.Forms.DockStyle.Fill;
            this.label2.Location = new System.Drawing.Point(23, 60);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(94, 40);
            this.label2.TabIndex = 1;
            this.label2.Text = "Name";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Dock = System.Windows.Forms.DockStyle.Fill;
            this.label3.Location = new System.Drawing.Point(23, 100);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(94, 40);
            this.label3.TabIndex = 2;
            this.label3.Text = "Folder";
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Dock = System.Windows.Forms.DockStyle.Fill;
            this.label4.Location = new System.Drawing.Point(23, 140);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(94, 84);
            this.label4.TabIndex = 3;
            this.label4.Text = "File Types";
            // 
            // NameTextBox
            // 
            this.tableLayoutPanel1.SetColumnSpan(this.NameTextBox, 2);
            this.NameTextBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.NameTextBox.Location = new System.Drawing.Point(144, 63);
            this.NameTextBox.Name = "NameTextBox";
            this.NameTextBox.Size = new System.Drawing.Size(302, 20);
            this.NameTextBox.TabIndex = 1;
            // 
            // FolderTextBox
            // 
            this.tableLayoutPanel1.SetColumnSpan(this.FolderTextBox, 2);
            this.FolderTextBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.FolderTextBox.Location = new System.Drawing.Point(144, 103);
            this.FolderTextBox.Name = "FolderTextBox";
            this.FolderTextBox.Size = new System.Drawing.Size(302, 20);
            this.FolderTextBox.TabIndex = 2;
            this.FolderTextBox.TextChanged += new System.EventHandler(this.FolderTextBox_TextChanged);
            this.FolderTextBox.Enter += new System.EventHandler(this.SelectFolder);
            // 
            // FileTypesCheckedListBox
            // 
            this.FileTypesCheckedListBox.BackColor = System.Drawing.SystemColors.Control;
            this.FileTypesCheckedListBox.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.FileTypesCheckedListBox.CheckOnClick = true;
            this.tableLayoutPanel1.SetColumnSpan(this.FileTypesCheckedListBox, 2);
            this.FileTypesCheckedListBox.FormattingEnabled = true;
            this.FileTypesCheckedListBox.Items.AddRange(new object[] {
            "*.cob / *.cbl",
            "*.txt",
            "*.src"});
            this.FileTypesCheckedListBox.Location = new System.Drawing.Point(144, 143);
            this.FileTypesCheckedListBox.Name = "FileTypesCheckedListBox";
            this.FileTypesCheckedListBox.Size = new System.Drawing.Size(302, 60);
            this.FileTypesCheckedListBox.TabIndex = 3;
            this.FileTypesCheckedListBox.MouseUp += new System.Windows.Forms.MouseEventHandler(this.FileTypesCheckedListBox_ItemCheck);
            // 
            // ProgressBar
            // 
            this.ProgressBar.Location = new System.Drawing.Point(23, 227);
            this.ProgressBar.Name = "ProgressBar";
            this.ProgressBar.Size = new System.Drawing.Size(94, 21);
            this.ProgressBar.TabIndex = 9;
            this.ProgressBar.Visible = false;
            // 
            // ProgressLabel
            // 
            this.ProgressLabel.AutoSize = true;
            this.ProgressLabel.Location = new System.Drawing.Point(144, 224);
            this.ProgressLabel.Name = "ProgressLabel";
            this.ProgressLabel.Size = new System.Drawing.Size(0, 13);
            this.ProgressLabel.TabIndex = 10;
            // 
            // StartAnalysisButton
            // 
            this.StartAnalysisButton.Location = new System.Drawing.Point(314, 227);
            this.StartAnalysisButton.Name = "StartAnalysisButton";
            this.StartAnalysisButton.Size = new System.Drawing.Size(129, 23);
            this.StartAnalysisButton.TabIndex = 4;
            this.StartAnalysisButton.Text = "Start Analysis";
            this.StartAnalysisButton.UseVisualStyleBackColor = true;
            this.StartAnalysisButton.Click += new System.EventHandler(this.StartAnalysisButton_Click);
            // 
            // CancelButton1
            // 
            this.CancelButton1.Location = new System.Drawing.Point(452, 227);
            this.CancelButton1.Name = "CancelButton1";
            this.CancelButton1.Size = new System.Drawing.Size(129, 23);
            this.CancelButton1.TabIndex = 8;
            this.CancelButton1.Text = "Cancel";
            this.CancelButton1.UseVisualStyleBackColor = true;
            this.CancelButton1.Click += new System.EventHandler(this.CancelButton1_Click);
            // 
            // pictureBox1
            // 
            this.pictureBox1.Image = ((System.Drawing.Image)(resources.GetObject("pictureBox1.Image")));
            this.pictureBox1.InitialImage = null;
            this.pictureBox1.Location = new System.Drawing.Point(452, 103);
            this.pictureBox1.Name = "pictureBox1";
            this.pictureBox1.Size = new System.Drawing.Size(26, 20);
            this.pictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom;
            this.pictureBox1.TabIndex = 11;
            this.pictureBox1.TabStop = false;
            this.pictureBox1.Click += new System.EventHandler(this.SelectFolder);
            // 
            // ProjectAssistant
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(597, 272);
            this.Controls.Add(this.tableLayoutPanel1);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "ProjectAssistant";
            this.Text = "New Project";
            this.tableLayoutPanel1.ResumeLayout(false);
            this.tableLayoutPanel1.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).EndInit();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.TableLayoutPanel tableLayoutPanel1;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.TextBox NameTextBox;
        private System.Windows.Forms.TextBox FolderTextBox;
        private System.Windows.Forms.CheckedListBox FileTypesCheckedListBox;
        private System.Windows.Forms.Button StartAnalysisButton;
        private System.Windows.Forms.Button CancelButton1;
        private System.Windows.Forms.ProgressBar ProgressBar;
        private System.Windows.Forms.FolderBrowserDialog folderBrowserDialog;
        private System.Windows.Forms.Label ProgressLabel;
        private System.Windows.Forms.PictureBox pictureBox1;
    }
}