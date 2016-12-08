using Canal.Properties;

namespace Canal
{
    partial class Log
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
            this.components = new System.ComponentModel.Container();
            this.levelFilter = new System.Windows.Forms.ComboBox();
            this.loggerEventArgsBindingSource = new System.Windows.Forms.BindingSource(this.components);
            this.logTextBox = new System.Windows.Forms.TextBox();
            ((System.ComponentModel.ISupportInitialize)(this.loggerEventArgsBindingSource)).BeginInit();
            this.SuspendLayout();
            // 
            // levelFilter
            // 
            this.levelFilter.FormattingEnabled = true;
            this.levelFilter.Items.AddRange(new object[] {
            "All",
            "Info",
            "Warning",
            "Error"});
            this.levelFilter.Location = new System.Drawing.Point(675, 12);
            this.levelFilter.Name = "levelFilter";
            this.levelFilter.Size = new System.Drawing.Size(121, 21);
            this.levelFilter.TabIndex = 0;
            this.levelFilter.SelectedIndexChanged += new System.EventHandler(this.comboBox1_SelectedIndexChanged);
            // 
            // loggerEventArgsBindingSource
            // 
            this.loggerEventArgsBindingSource.DataSource = typeof(Logging.LoggerEventArgs);
            // 
            // logTextBox
            // 
            this.logTextBox.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.logTextBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.logTextBox.Location = new System.Drawing.Point(0, 0);
            this.logTextBox.Multiline = true;
            this.logTextBox.Name = "logTextBox";
            this.logTextBox.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.logTextBox.Size = new System.Drawing.Size(826, 488);
            this.logTextBox.TabIndex = 1;
            // 
            // Log
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(826, 488);
            this.Controls.Add(this.levelFilter);
            this.Controls.Add(this.logTextBox);
            this.Icon = Resources.Canal_Logo_small;
            this.Name = "Log";
            this.Text = "Log";
            ((System.ComponentModel.ISupportInitialize)(this.loggerEventArgsBindingSource)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion
        private System.Windows.Forms.ComboBox levelFilter;
        private System.Windows.Forms.BindingSource loggerEventArgsBindingSource;
        private System.Windows.Forms.TextBox logTextBox;
    }
}