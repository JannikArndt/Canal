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
            this.components = new System.ComponentModel.Container();
            this.imageList1 = new System.Windows.Forms.ImageList(this.components);
            this.tableLayoutPanel2 = new System.Windows.Forms.TableLayoutPanel();
            this.pictureBox1 = new System.Windows.Forms.PictureBox();
            this.label3 = new System.Windows.Forms.Label();
            this.changeLogTextBox = new System.Windows.Forms.TextBox();
            this.label2 = new System.Windows.Forms.Label();
            this.recentFilesListView = new Canal.UserControls.FileListBox();
            this.tableLayoutPanel2.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).BeginInit();
            this.SuspendLayout();
            // 
            // imageList1
            // 
            this.imageList1.ColorDepth = System.Windows.Forms.ColorDepth.Depth8Bit;
            this.imageList1.ImageSize = new System.Drawing.Size(2, 30);
            this.imageList1.TransparentColor = System.Drawing.Color.Transparent;
            // 
            // tableLayoutPanel2
            // 
            this.tableLayoutPanel2.ColumnCount = 2;
            this.tableLayoutPanel2.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 50F));
            this.tableLayoutPanel2.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 50F));
            this.tableLayoutPanel2.Controls.Add(this.label3, 0, 2);
            this.tableLayoutPanel2.Controls.Add(this.changeLogTextBox, 0, 3);
            this.tableLayoutPanel2.Controls.Add(this.pictureBox1, 0, 0);
            this.tableLayoutPanel2.Controls.Add(this.label2, 1, 0);
            this.tableLayoutPanel2.Controls.Add(this.recentFilesListView, 1, 1);
            this.tableLayoutPanel2.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tableLayoutPanel2.Location = new System.Drawing.Point(0, 0);
            this.tableLayoutPanel2.Name = "tableLayoutPanel2";
            this.tableLayoutPanel2.RowCount = 4;
            this.tableLayoutPanel2.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 40F));
            this.tableLayoutPanel2.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 50F));
            this.tableLayoutPanel2.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 40F));
            this.tableLayoutPanel2.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 50F));
            this.tableLayoutPanel2.Size = new System.Drawing.Size(1148, 594);
            this.tableLayoutPanel2.TabIndex = 8;
            // 
            // pictureBox1
            // 
            this.pictureBox1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.pictureBox1.Image = global::Canal.Properties.Resources.Canal_Logo_big;
            this.pictureBox1.Location = new System.Drawing.Point(3, 3);
            this.pictureBox1.Name = "pictureBox1";
            this.tableLayoutPanel2.SetRowSpan(this.pictureBox1, 2);
            this.pictureBox1.Size = new System.Drawing.Size(568, 291);
            this.pictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom;
            this.pictureBox1.TabIndex = 6;
            this.pictureBox1.TabStop = false;
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Dock = System.Windows.Forms.DockStyle.Fill;
            this.label3.Font = new System.Drawing.Font("Microsoft Sans Serif", 14.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label3.Location = new System.Drawing.Point(3, 297);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(568, 40);
            this.label3.TabIndex = 12;
            this.label3.Text = "New in this Version";
            this.label3.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            // 
            // changeLogTextBox
            // 
            this.changeLogTextBox.BackColor = System.Drawing.SystemColors.Control;
            this.changeLogTextBox.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.changeLogTextBox.Cursor = System.Windows.Forms.Cursors.Arrow;
            this.changeLogTextBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.changeLogTextBox.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.changeLogTextBox.Location = new System.Drawing.Point(3, 340);
            this.changeLogTextBox.Multiline = true;
            this.changeLogTextBox.Name = "changeLogTextBox";
            this.changeLogTextBox.ReadOnly = true;
            this.changeLogTextBox.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.changeLogTextBox.Size = new System.Drawing.Size(568, 251);
            this.changeLogTextBox.TabIndex = 7;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Dock = System.Windows.Forms.DockStyle.Fill;
            this.label2.Font = new System.Drawing.Font("Microsoft Sans Serif", 14.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label2.Location = new System.Drawing.Point(577, 0);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(568, 40);
            this.label2.TabIndex = 8;
            this.label2.Text = "Recent Files";
            this.label2.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            // 
            // recentFilesListView
            // 
            this.recentFilesListView.BackColor = System.Drawing.SystemColors.Control;
            this.recentFilesListView.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.recentFilesListView.Cursor = System.Windows.Forms.Cursors.Hand;
            this.recentFilesListView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.recentFilesListView.DrawMode = System.Windows.Forms.DrawMode.OwnerDrawVariable;
            this.recentFilesListView.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.recentFilesListView.FormattingEnabled = true;
            this.recentFilesListView.ItemHeight = 16;
            this.recentFilesListView.Location = new System.Drawing.Point(577, 43);
            this.recentFilesListView.Name = "recentFilesListView";
            this.tableLayoutPanel2.SetRowSpan(this.recentFilesListView, 3);
            this.recentFilesListView.Size = new System.Drawing.Size(568, 548);
            this.recentFilesListView.TabIndex = 9;
            this.recentFilesListView.DoubleClick += new System.EventHandler(this.recentFilesListView_DoubleClick);
            // 
            // FirstTabPage
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.tableLayoutPanel2);
            this.Name = "FirstTabPage";
            this.Size = new System.Drawing.Size(1148, 594);
            this.tableLayoutPanel2.ResumeLayout(false);
            this.tableLayoutPanel2.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).EndInit();
            this.ResumeLayout(false);

        }

        #endregion
        private System.Windows.Forms.ImageList imageList1;
        private System.Windows.Forms.TableLayoutPanel tableLayoutPanel2;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.TextBox changeLogTextBox;
        private FileListBox recentFilesListView;
        private System.Windows.Forms.PictureBox pictureBox1;
        private System.Windows.Forms.Label label2;
    }
}
