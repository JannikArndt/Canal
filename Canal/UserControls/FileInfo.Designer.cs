namespace Canal.UserControls
{
    partial class FileInfo
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
            this.callReferencesListBox = new System.Windows.Forms.ListBox();
            this.infoDataGridView = new System.Windows.Forms.DataGridView();
            this.label1 = new System.Windows.Forms.Label();
            this.splitContainer = new System.Windows.Forms.SplitContainer();
            ((System.ComponentModel.ISupportInitialize)(this.infoDataGridView)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer)).BeginInit();
            this.splitContainer.Panel1.SuspendLayout();
            this.splitContainer.SuspendLayout();
            this.SuspendLayout();
            // 
            // headingLabel
            // 
            this.headingLabel.AutoSize = true;
            this.headingLabel.Font = new System.Drawing.Font("Consolas", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.headingLabel.Location = new System.Drawing.Point(3, 6);
            this.headingLabel.Name = "headingLabel";
            this.headingLabel.Size = new System.Drawing.Size(117, 19);
            this.headingLabel.TabIndex = 1;
            this.headingLabel.Text = "headingLabel";
            // 
            // callReferencesListBox
            // 
            this.callReferencesListBox.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.callReferencesListBox.BackColor = System.Drawing.SystemColors.Window;
            this.callReferencesListBox.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.callReferencesListBox.FormattingEnabled = true;
            this.callReferencesListBox.Location = new System.Drawing.Point(0, 249);
            this.callReferencesListBox.Name = "callReferencesListBox";
            this.callReferencesListBox.Size = new System.Drawing.Size(326, 156);
            this.callReferencesListBox.TabIndex = 2;
            this.callReferencesListBox.DoubleClick += new System.EventHandler(this.callReferencesListBox_DoubleClick);
            // 
            // infoDataGridView
            // 
            this.infoDataGridView.AllowUserToAddRows = false;
            this.infoDataGridView.AllowUserToDeleteRows = false;
            this.infoDataGridView.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.infoDataGridView.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill;
            this.infoDataGridView.BackgroundColor = System.Drawing.SystemColors.Window;
            this.infoDataGridView.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.infoDataGridView.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.infoDataGridView.ColumnHeadersVisible = false;
            this.infoDataGridView.GridColor = System.Drawing.SystemColors.ControlLightLight;
            this.infoDataGridView.Location = new System.Drawing.Point(2, 37);
            this.infoDataGridView.Name = "infoDataGridView";
            this.infoDataGridView.ReadOnly = true;
            this.infoDataGridView.RowHeadersBorderStyle = System.Windows.Forms.DataGridViewHeaderBorderStyle.Single;
            this.infoDataGridView.RowHeadersVisible = false;
            this.infoDataGridView.RowTemplate.DefaultCellStyle.BackColor = System.Drawing.Color.White;
            this.infoDataGridView.RowTemplate.DefaultCellStyle.Font = new System.Drawing.Font("Calibri", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.infoDataGridView.RowTemplate.DefaultCellStyle.ForeColor = System.Drawing.Color.Black;
            this.infoDataGridView.RowTemplate.DefaultCellStyle.NullValue = "-";
            this.infoDataGridView.RowTemplate.DefaultCellStyle.SelectionBackColor = System.Drawing.Color.White;
            this.infoDataGridView.RowTemplate.DefaultCellStyle.SelectionForeColor = System.Drawing.Color.Black;
            this.infoDataGridView.RowTemplate.ReadOnly = true;
            this.infoDataGridView.ShowCellErrors = false;
            this.infoDataGridView.ShowCellToolTips = false;
            this.infoDataGridView.ShowEditingIcon = false;
            this.infoDataGridView.ShowRowErrors = false;
            this.infoDataGridView.Size = new System.Drawing.Size(324, 179);
            this.infoDataGridView.TabIndex = 3;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label1.Location = new System.Drawing.Point(3, 226);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(97, 13);
            this.label1.TabIndex = 4;
            this.label1.Text = "Call References";
            // 
            // splitContainer
            // 
            this.splitContainer.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer.Location = new System.Drawing.Point(0, 0);
            this.splitContainer.Name = "splitContainer";
            this.splitContainer.Orientation = System.Windows.Forms.Orientation.Horizontal;
            // 
            // splitContainer.Panel1
            // 
            this.splitContainer.Panel1.Controls.Add(this.infoDataGridView);
            this.splitContainer.Panel1.Controls.Add(this.label1);
            this.splitContainer.Panel1.Controls.Add(this.headingLabel);
            this.splitContainer.Panel1.Controls.Add(this.callReferencesListBox);
            this.splitContainer.Size = new System.Drawing.Size(326, 709);
            this.splitContainer.SplitterDistance = 424;
            this.splitContainer.TabIndex = 5;
            // 
            // FileInfo
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackColor = System.Drawing.SystemColors.Window;
            this.Controls.Add(this.splitContainer);
            this.Name = "FileInfo";
            this.Size = new System.Drawing.Size(326, 709);
            ((System.ComponentModel.ISupportInitialize)(this.infoDataGridView)).EndInit();
            this.splitContainer.Panel1.ResumeLayout(false);
            this.splitContainer.Panel1.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer)).EndInit();
            this.splitContainer.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Label headingLabel;
        private System.Windows.Forms.ListBox callReferencesListBox;
        private System.Windows.Forms.DataGridView infoDataGridView;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.SplitContainer splitContainer;
    }
}
