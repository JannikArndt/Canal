namespace Canal.UserControls.WordInfoViews
{
    partial class ProcedureInfo
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
            this.tableLayoutPanel1 = new System.Windows.Forms.TableLayoutPanel();
            this.ReferencedByList = new System.Windows.Forms.ListBox();
            this.GoTosList = new System.Windows.Forms.ListBox();
            this.CallsList = new System.Windows.Forms.ListBox();
            this.label7 = new System.Windows.Forms.Label();
            this.label5 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.label1 = new System.Windows.Forms.Label();
            this.PerformsList = new System.Windows.Forms.ListBox();
            this.label2 = new System.Windows.Forms.Label();
            this.LinesOfCodeText = new System.Windows.Forms.Label();
            this.splitContainer1 = new System.Windows.Forms.SplitContainer();
            this.variableTreeView = new Canal.UserControls.WordInfoViews.VariableTreeView();
            this.tableLayoutPanel1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer1)).BeginInit();
            this.splitContainer1.Panel1.SuspendLayout();
            this.splitContainer1.Panel2.SuspendLayout();
            this.splitContainer1.SuspendLayout();
            this.SuspendLayout();
            // 
            // tableLayoutPanel1
            // 
            this.tableLayoutPanel1.AutoScroll = true;
            this.tableLayoutPanel1.AutoSize = true;
            this.tableLayoutPanel1.ColumnCount = 2;
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 138F));
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 241F));
            this.tableLayoutPanel1.Controls.Add(this.ReferencedByList, 1, 3);
            this.tableLayoutPanel1.Controls.Add(this.GoTosList, 1, 1);
            this.tableLayoutPanel1.Controls.Add(this.CallsList, 1, 2);
            this.tableLayoutPanel1.Controls.Add(this.label7, 0, 4);
            this.tableLayoutPanel1.Controls.Add(this.label5, 0, 3);
            this.tableLayoutPanel1.Controls.Add(this.label3, 0, 2);
            this.tableLayoutPanel1.Controls.Add(this.label1, 0, 0);
            this.tableLayoutPanel1.Controls.Add(this.PerformsList, 1, 0);
            this.tableLayoutPanel1.Controls.Add(this.label2, 0, 1);
            this.tableLayoutPanel1.Controls.Add(this.LinesOfCodeText, 1, 4);
            this.tableLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tableLayoutPanel1.GrowStyle = System.Windows.Forms.TableLayoutPanelGrowStyle.FixedSize;
            this.tableLayoutPanel1.Location = new System.Drawing.Point(0, 0);
            this.tableLayoutPanel1.Name = "tableLayoutPanel1";
            this.tableLayoutPanel1.RowCount = 5;
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 40F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 40F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 40F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 40F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 40F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tableLayoutPanel1.Size = new System.Drawing.Size(379, 216);
            this.tableLayoutPanel1.TabIndex = 0;
            // 
            // ReferencedByList
            // 
            this.ReferencedByList.BackColor = System.Drawing.SystemColors.Control;
            this.ReferencedByList.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.ReferencedByList.Cursor = System.Windows.Forms.Cursors.Hand;
            this.ReferencedByList.Dock = System.Windows.Forms.DockStyle.Fill;
            this.ReferencedByList.FormattingEnabled = true;
            this.ReferencedByList.Location = new System.Drawing.Point(141, 123);
            this.ReferencedByList.Name = "ReferencedByList";
            this.ReferencedByList.Size = new System.Drawing.Size(235, 34);
            this.ReferencedByList.TabIndex = 10;
            this.ReferencedByList.DoubleClick += new System.EventHandler(this.ProcedureList_DoubleClick);
            // 
            // GoTosList
            // 
            this.GoTosList.BackColor = System.Drawing.SystemColors.Control;
            this.GoTosList.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.GoTosList.Cursor = System.Windows.Forms.Cursors.Hand;
            this.GoTosList.Dock = System.Windows.Forms.DockStyle.Fill;
            this.GoTosList.FormattingEnabled = true;
            this.GoTosList.Location = new System.Drawing.Point(141, 43);
            this.GoTosList.Name = "GoTosList";
            this.GoTosList.Size = new System.Drawing.Size(235, 34);
            this.GoTosList.TabIndex = 11;
            this.GoTosList.DoubleClick += new System.EventHandler(this.ProcedureList_DoubleClick);
            // 
            // CallsList
            // 
            this.CallsList.BackColor = System.Drawing.SystemColors.Control;
            this.CallsList.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.CallsList.Cursor = System.Windows.Forms.Cursors.Hand;
            this.CallsList.Dock = System.Windows.Forms.DockStyle.Fill;
            this.CallsList.FormattingEnabled = true;
            this.CallsList.Location = new System.Drawing.Point(141, 83);
            this.CallsList.Name = "CallsList";
            this.CallsList.Size = new System.Drawing.Size(235, 34);
            this.CallsList.TabIndex = 9;
            this.CallsList.DoubleClick += new System.EventHandler(this.ProcedureList_DoubleClick);
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Dock = System.Windows.Forms.DockStyle.Fill;
            this.label7.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label7.Location = new System.Drawing.Point(3, 160);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(132, 56);
            this.label7.TabIndex = 6;
            this.label7.Text = "Lines of Code";
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Dock = System.Windows.Forms.DockStyle.Fill;
            this.label5.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label5.Location = new System.Drawing.Point(3, 120);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(132, 40);
            this.label5.TabIndex = 4;
            this.label5.Text = "Referenced By";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Dock = System.Windows.Forms.DockStyle.Fill;
            this.label3.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label3.Location = new System.Drawing.Point(3, 80);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(132, 40);
            this.label3.TabIndex = 2;
            this.label3.Text = "Calls";
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.label1.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label1.Location = new System.Drawing.Point(3, 0);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(132, 40);
            this.label1.TabIndex = 0;
            this.label1.Text = "Performs";
            // 
            // PerformsList
            // 
            this.PerformsList.BackColor = System.Drawing.SystemColors.Control;
            this.PerformsList.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.PerformsList.Cursor = System.Windows.Forms.Cursors.Hand;
            this.PerformsList.Dock = System.Windows.Forms.DockStyle.Fill;
            this.PerformsList.FormattingEnabled = true;
            this.PerformsList.IntegralHeight = false;
            this.PerformsList.Location = new System.Drawing.Point(141, 3);
            this.PerformsList.Name = "PerformsList";
            this.PerformsList.Size = new System.Drawing.Size(235, 34);
            this.PerformsList.TabIndex = 8;
            this.PerformsList.DoubleClick += new System.EventHandler(this.ProcedureList_DoubleClick);
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Dock = System.Windows.Forms.DockStyle.Fill;
            this.label2.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label2.Location = new System.Drawing.Point(3, 40);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(132, 40);
            this.label2.TabIndex = 12;
            this.label2.Text = "GO TOs";
            // 
            // LinesOfCodeText
            // 
            this.LinesOfCodeText.AutoSize = true;
            this.LinesOfCodeText.Location = new System.Drawing.Point(141, 160);
            this.LinesOfCodeText.Name = "LinesOfCodeText";
            this.LinesOfCodeText.Size = new System.Drawing.Size(74, 13);
            this.LinesOfCodeText.TabIndex = 13;
            this.LinesOfCodeText.Text = "Lines Of Code";
            this.LinesOfCodeText.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // splitContainer1
            // 
            this.splitContainer1.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.splitContainer1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer1.Location = new System.Drawing.Point(0, 0);
            this.splitContainer1.Name = "splitContainer1";
            this.splitContainer1.Orientation = System.Windows.Forms.Orientation.Horizontal;
            // 
            // splitContainer1.Panel1
            // 
            this.splitContainer1.Panel1.Controls.Add(this.tableLayoutPanel1);
            this.splitContainer1.Panel1MinSize = 0;
            // 
            // splitContainer1.Panel2
            // 
            this.splitContainer1.Panel2.Controls.Add(this.variableTreeView);
            this.splitContainer1.Panel2MinSize = 0;
            this.splitContainer1.Size = new System.Drawing.Size(381, 437);
            this.splitContainer1.SplitterDistance = 218;
            this.splitContainer1.TabIndex = 1;
            // 
            // variableTreeView
            // 
            this.variableTreeView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.variableTreeView.Location = new System.Drawing.Point(0, 0);
            this.variableTreeView.Name = "variableTreeView";
            this.variableTreeView.Size = new System.Drawing.Size(379, 213);
            this.variableTreeView.TabIndex = 0;
            // 
            // ProcedureInfo
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoScroll = true;
            this.AutoSize = true;
            this.Controls.Add(this.splitContainer1);
            this.Name = "ProcedureInfo";
            this.Size = new System.Drawing.Size(381, 437);
            this.tableLayoutPanel1.ResumeLayout(false);
            this.tableLayoutPanel1.PerformLayout();
            this.splitContainer1.Panel1.ResumeLayout(false);
            this.splitContainer1.Panel1.PerformLayout();
            this.splitContainer1.Panel2.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer1)).EndInit();
            this.splitContainer1.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.TableLayoutPanel tableLayoutPanel1;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.ListBox PerformsList;
        private System.Windows.Forms.ListBox CallsList;
        private System.Windows.Forms.ListBox ReferencedByList;
        private System.Windows.Forms.ListBox GoTosList;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label LinesOfCodeText;
        private System.Windows.Forms.SplitContainer splitContainer1;
        private VariableTreeView variableTreeView;
    }
}
