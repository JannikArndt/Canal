using Canal.Properties;

namespace Canal.UserControls
{
    partial class FileReferenceFilter
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(FileReferenceFilter));
            this.referenceListBox = new System.Windows.Forms.CheckedListBox();
            this.button_selectAll = new System.Windows.Forms.Button();
            this.button_selectNone = new System.Windows.Forms.Button();
            this.button_ok = new System.Windows.Forms.Button();
            this.button_cancel = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // referenceListBox
            // 
            this.referenceListBox.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.referenceListBox.CheckOnClick = true;
            this.referenceListBox.ColumnWidth = 5;
            this.referenceListBox.Font = new System.Drawing.Font("Source Code Pro Light", 9.749999F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.referenceListBox.FormattingEnabled = true;
            this.referenceListBox.Items.AddRange(new object[] {
            "MYFOLDER > COPYBOOK1",
            "MYFOLDER > COPYBOOK2",
            "MYFOLDER > COPYBOOK3"});
            this.referenceListBox.Location = new System.Drawing.Point(12, 12);
            this.referenceListBox.Margin = new System.Windows.Forms.Padding(5);
            this.referenceListBox.Name = "referenceListBox";
            this.referenceListBox.Size = new System.Drawing.Size(512, 365);
            this.referenceListBox.TabIndex = 0;
            // 
            // button_selectAll
            // 
            this.button_selectAll.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.button_selectAll.Location = new System.Drawing.Point(12, 410);
            this.button_selectAll.Name = "button_selectAll";
            this.button_selectAll.Size = new System.Drawing.Size(75, 23);
            this.button_selectAll.TabIndex = 3;
            this.button_selectAll.Text = "Select All";
            this.button_selectAll.UseVisualStyleBackColor = true;
            this.button_selectAll.Click += new System.EventHandler(this.button_selectAll_Click);
            // 
            // button_selectNone
            // 
            this.button_selectNone.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.button_selectNone.Location = new System.Drawing.Point(93, 410);
            this.button_selectNone.Name = "button_selectNone";
            this.button_selectNone.Size = new System.Drawing.Size(75, 23);
            this.button_selectNone.TabIndex = 2;
            this.button_selectNone.Text = "Select None";
            this.button_selectNone.UseVisualStyleBackColor = true;
            this.button_selectNone.Click += new System.EventHandler(this.button_selectNone_Click);
            // 
            // button_ok
            // 
            this.button_ok.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.button_ok.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.button_ok.Location = new System.Drawing.Point(368, 410);
            this.button_ok.Name = "button_ok";
            this.button_ok.Size = new System.Drawing.Size(75, 23);
            this.button_ok.TabIndex = 1;
            this.button_ok.Text = "OK";
            this.button_ok.UseVisualStyleBackColor = true;
            this.button_ok.Click += new System.EventHandler(this.button_ok_Click);
            // 
            // button_cancel
            // 
            this.button_cancel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.button_cancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.button_cancel.Location = new System.Drawing.Point(449, 410);
            this.button_cancel.Name = "button_cancel";
            this.button_cancel.Size = new System.Drawing.Size(75, 23);
            this.button_cancel.TabIndex = 2;
            this.button_cancel.Text = "Cancel";
            this.button_cancel.UseVisualStyleBackColor = true;
            this.button_cancel.Click += new System.EventHandler(this.button_cancel_Click);
            // 
            // FileReferenceFilter
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(536, 445);
            this.Controls.Add(this.button_cancel);
            this.Controls.Add(this.button_ok);
            this.Controls.Add(this.button_selectNone);
            this.Controls.Add(this.button_selectAll);
            this.Controls.Add(this.referenceListBox);
            this.Icon = Resources.Canal_Logo_small;
            this.Name = "FileReferenceFilter";
            this.Text = "FileReferenceFilter";
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.CheckedListBox referenceListBox;
        private System.Windows.Forms.Button button_selectAll;
        private System.Windows.Forms.Button button_selectNone;
        private System.Windows.Forms.Button button_ok;
        private System.Windows.Forms.Button button_cancel;
    }
}