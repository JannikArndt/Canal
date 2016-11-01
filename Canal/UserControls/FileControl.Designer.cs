using FastColoredTextBoxNS;
using System.Windows.Forms;

namespace Canal.UserControls
{
    sealed partial class FileControl
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
        /// <param name="file"></param>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(FileControl));
            this.codeBox = new FastColoredTextBoxNS.FastColoredTextBox();
            this.splitContainerTop = new System.Windows.Forms.SplitContainer();
            this.codeViewToolStrip = new System.Windows.Forms.ToolStrip();
            this.navigateBackward = new System.Windows.Forms.ToolStripButton();
            this.navigateForwardButton = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator2 = new System.Windows.Forms.ToolStripSeparator();
            this.newButton = new System.Windows.Forms.ToolStripButton();
            this.openButton = new System.Windows.Forms.ToolStripButton();
            this.saveButton = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator4 = new System.Windows.Forms.ToolStripSeparator();
            this.undoButton = new System.Windows.Forms.ToolStripButton();
            this.redoButton = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator3 = new System.Windows.Forms.ToolStripSeparator();
            this.searchBox = new System.Windows.Forms.ToolStripTextBox();
            this.findPreviousButton = new System.Windows.Forms.ToolStripButton();
            this.findNextButton = new System.Windows.Forms.ToolStripButton();
            this.searchWithRegEx = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.formatCodeButton = new System.Windows.Forms.ToolStripButton();
            this.splitContainerRight = new System.Windows.Forms.SplitContainer();
            this.tableOfContents = new Canal.UserControls.TableOfContents();
            this.loaderImageInfoTab = new System.Windows.Forms.PictureBox();
            this.miniToolStrip = new System.Windows.Forms.ToolStrip();
            ((System.ComponentModel.ISupportInitialize)(this.codeBox)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainerTop)).BeginInit();
            this.splitContainerTop.Panel1.SuspendLayout();
            this.splitContainerTop.Panel2.SuspendLayout();
            this.splitContainerTop.SuspendLayout();
            this.codeViewToolStrip.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainerRight)).BeginInit();
            this.splitContainerRight.Panel1.SuspendLayout();
            this.splitContainerRight.Panel2.SuspendLayout();
            this.splitContainerRight.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.loaderImageInfoTab)).BeginInit();
            this.SuspendLayout();
            // 
            // codeBox
            // 
            this.codeBox.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.codeBox.AutoCompleteBracketsList = new char[] {
        '(',
        ')',
        '{',
        '}',
        '[',
        ']',
        '\"',
        '\"',
        '\'',
        '\''};
            this.codeBox.AutoIndent = false;
            this.codeBox.AutoIndentChars = false;
            this.codeBox.AutoIndentCharsPatterns = "^\\s*[\\w\\.]+(\\s\\w+)?\\s*(?<range>=)\\s*(?<range>.+)";
            this.codeBox.AutoIndentExistingLines = false;
            this.codeBox.AutoScrollMinSize = new System.Drawing.Size(45, 15);
            this.codeBox.BackBrush = null;
            this.codeBox.BracketsHighlightStrategy = FastColoredTextBoxNS.Enums.BracketsHighlightStrategy.Strategy2;
            this.codeBox.CharHeight = 15;
            this.codeBox.CharWidth = 7;
            this.codeBox.Cursor = System.Windows.Forms.Cursors.IBeam;
            this.codeBox.DisabledColor = System.Drawing.Color.FromArgb(((int)(((byte)(100)))), ((int)(((byte)(180)))), ((int)(((byte)(180)))), ((int)(((byte)(180)))));
            this.codeBox.Font = new System.Drawing.Font("Consolas", 9.75F);
            this.codeBox.HighlightingRangeType = FastColoredTextBoxNS.Enums.HighlightingRangeType.VisibleRange;
            this.codeBox.Hotkeys = resources.GetString("codeBox.Hotkeys");
            this.codeBox.IsReplaceMode = false;
            this.codeBox.Language = FastColoredTextBoxNS.Enums.Language.Cobol;
            this.codeBox.LeftBracket = '(';
            this.codeBox.LeftBracket2 = '{';
            this.codeBox.Location = new System.Drawing.Point(0, 27);
            this.codeBox.Margin = new System.Windows.Forms.Padding(2);
            this.codeBox.Name = "codeBox";
            this.codeBox.Paddings = new System.Windows.Forms.Padding(0);
            this.codeBox.RightBracket = ')';
            this.codeBox.RightBracket2 = '}';
            this.codeBox.SelectionColor = System.Drawing.Color.FromArgb(((int)(((byte)(60)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(255)))));
            this.codeBox.ServiceColors = ((FastColoredTextBoxNS.ServiceColors)(resources.GetObject("codeBox.ServiceColors")));
            this.codeBox.Size = new System.Drawing.Size(679, 556);
            this.codeBox.TabIndex = 0;
            this.codeBox.Zoom = 100;
            // 
            // splitContainerTop
            // 
            this.splitContainerTop.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainerTop.Location = new System.Drawing.Point(0, 0);
            this.splitContainerTop.Name = "splitContainerTop";
            // 
            // splitContainerTop.Panel1
            // 
            this.splitContainerTop.Panel1.Controls.Add(this.codeViewToolStrip);
            this.splitContainerTop.Panel1.Controls.Add(this.codeBox);
            // 
            // splitContainerTop.Panel2
            // 
            this.splitContainerTop.Panel2.Controls.Add(this.splitContainerRight);
            this.splitContainerTop.Size = new System.Drawing.Size(1061, 583);
            this.splitContainerTop.SplitterDistance = 679;
            this.splitContainerTop.TabIndex = 6;
            // 
            // codeViewToolStrip
            // 
            this.codeViewToolStrip.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
            this.codeViewToolStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.navigateBackward,
            this.navigateForwardButton,
            this.toolStripSeparator2,
            this.newButton,
            this.openButton,
            this.saveButton,
            this.toolStripSeparator4,
            this.undoButton,
            this.redoButton,
            this.toolStripSeparator3,
            this.searchBox,
            this.findPreviousButton,
            this.findNextButton,
            this.searchWithRegEx,
            this.toolStripSeparator1,
            this.formatCodeButton});
            this.codeViewToolStrip.Location = new System.Drawing.Point(0, 0);
            this.codeViewToolStrip.Name = "codeViewToolStrip";
            this.codeViewToolStrip.Size = new System.Drawing.Size(679, 25);
            this.codeViewToolStrip.TabIndex = 1;
            this.codeViewToolStrip.Text = "codeViewToolStrip";
            // 
            // navigateBackward
            // 
            this.navigateBackward.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.navigateBackward.Image = ((System.Drawing.Image)(resources.GetObject("navigateBackward.Image")));
            this.navigateBackward.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.navigateBackward.Name = "navigateBackward";
            this.navigateBackward.Size = new System.Drawing.Size(23, 22);
            this.navigateBackward.Text = "toolStripButton1";
            this.navigateBackward.ToolTipText = "Navigate Backward (Ctrl + -)";
            this.navigateBackward.Click += new System.EventHandler(this.NavigateBackwardClick);
            // 
            // navigateForwardButton
            // 
            this.navigateForwardButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.navigateForwardButton.Enabled = false;
            this.navigateForwardButton.Image = ((System.Drawing.Image)(resources.GetObject("navigateForwardButton.Image")));
            this.navigateForwardButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.navigateForwardButton.Name = "navigateForwardButton";
            this.navigateForwardButton.Size = new System.Drawing.Size(23, 22);
            this.navigateForwardButton.Text = "toolStripButton1";
            this.navigateForwardButton.ToolTipText = "Navigate Forward (Ctrl + Shift + -)";
            this.navigateForwardButton.Click += new System.EventHandler(this.NavigateForwardButtonClick);
            // 
            // toolStripSeparator2
            // 
            this.toolStripSeparator2.Name = "toolStripSeparator2";
            this.toolStripSeparator2.Size = new System.Drawing.Size(6, 25);
            // 
            // newButton
            // 
            this.newButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.newButton.Image = ((System.Drawing.Image)(resources.GetObject("newButton.Image")));
            this.newButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.newButton.Name = "newButton";
            this.newButton.Size = new System.Drawing.Size(23, 22);
            this.newButton.Text = "New";
            this.newButton.Click += new System.EventHandler(this.newButton_Click);
            // 
            // openButton
            // 
            this.openButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.openButton.Image = ((System.Drawing.Image)(resources.GetObject("openButton.Image")));
            this.openButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.openButton.Name = "openButton";
            this.openButton.Size = new System.Drawing.Size(23, 22);
            this.openButton.Text = "Open";
            this.openButton.Click += new System.EventHandler(this.openButton_Click);
            // 
            // saveButton
            // 
            this.saveButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.saveButton.Enabled = false;
            this.saveButton.Image = ((System.Drawing.Image)(resources.GetObject("saveButton.Image")));
            this.saveButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.saveButton.Name = "saveButton";
            this.saveButton.Size = new System.Drawing.Size(23, 22);
            this.saveButton.Text = "Save";
            this.saveButton.Click += new System.EventHandler(this.saveButton_Click);
            // 
            // toolStripSeparator4
            // 
            this.toolStripSeparator4.Name = "toolStripSeparator4";
            this.toolStripSeparator4.Size = new System.Drawing.Size(6, 25);
            // 
            // undoButton
            // 
            this.undoButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.undoButton.Enabled = false;
            this.undoButton.Image = ((System.Drawing.Image)(resources.GetObject("undoButton.Image")));
            this.undoButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.undoButton.Name = "undoButton";
            this.undoButton.Size = new System.Drawing.Size(23, 22);
            this.undoButton.Text = "Undo";
            this.undoButton.Click += new System.EventHandler(this.undoButton_Click);
            // 
            // redoButton
            // 
            this.redoButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.redoButton.Enabled = false;
            this.redoButton.Image = ((System.Drawing.Image)(resources.GetObject("redoButton.Image")));
            this.redoButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.redoButton.Name = "redoButton";
            this.redoButton.Size = new System.Drawing.Size(23, 22);
            this.redoButton.Text = "Redo";
            this.redoButton.Click += new System.EventHandler(this.redoButton_Click);
            // 
            // toolStripSeparator3
            // 
            this.toolStripSeparator3.Name = "toolStripSeparator3";
            this.toolStripSeparator3.Size = new System.Drawing.Size(6, 25);
            // 
            // searchBox
            // 
            this.searchBox.Name = "searchBox";
            this.searchBox.Size = new System.Drawing.Size(200, 25);
            this.searchBox.Text = "Search...";
            this.searchBox.Enter += new System.EventHandler(this.SearchBoxEnter);
            this.searchBox.Leave += new System.EventHandler(this.SearchBoxLeave);
            this.searchBox.KeyDown += new System.Windows.Forms.KeyEventHandler(this.HandleKeyDown);
            this.searchBox.TextChanged += new System.EventHandler(this.SeachBoxTextChanged);
            // 
            // findPreviousButton
            // 
            this.findPreviousButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.findPreviousButton.Image = ((System.Drawing.Image)(resources.GetObject("findPreviousButton.Image")));
            this.findPreviousButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.findPreviousButton.Name = "findPreviousButton";
            this.findPreviousButton.Size = new System.Drawing.Size(23, 22);
            this.findPreviousButton.Text = "Find Previous (Shift + F3)";
            this.findPreviousButton.Click += new System.EventHandler(this.FindPreviousButtonClick);
            // 
            // findNextButton
            // 
            this.findNextButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.findNextButton.Image = ((System.Drawing.Image)(resources.GetObject("findNextButton.Image")));
            this.findNextButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.findNextButton.Name = "findNextButton";
            this.findNextButton.Size = new System.Drawing.Size(23, 22);
            this.findNextButton.Text = "Find Next (F3)";
            this.findNextButton.Click += new System.EventHandler(this.FindNextButtonClick);
            // 
            // searchWithRegEx
            // 
            this.searchWithRegEx.CheckOnClick = true;
            this.searchWithRegEx.Image = ((System.Drawing.Image)(resources.GetObject("searchWithRegEx.Image")));
            this.searchWithRegEx.ImageTransparentColor = System.Drawing.Color.Black;
            this.searchWithRegEx.Name = "searchWithRegEx";
            this.searchWithRegEx.Size = new System.Drawing.Size(58, 22);
            this.searchWithRegEx.Text = "RegEx";
            // 
            // toolStripSeparator1
            // 
            this.toolStripSeparator1.Name = "toolStripSeparator1";
            this.toolStripSeparator1.Size = new System.Drawing.Size(6, 25);
            // 
            // formatCodeButton
            // 
            this.formatCodeButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.formatCodeButton.Image = ((System.Drawing.Image)(resources.GetObject("formatCodeButton.Image")));
            this.formatCodeButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.formatCodeButton.Name = "formatCodeButton";
            this.formatCodeButton.Size = new System.Drawing.Size(23, 22);
            this.formatCodeButton.Text = "Format Code";
            this.formatCodeButton.Click += new System.EventHandler(this.formatCodeButton_Click);
            // 
            // splitContainerRight
            // 
            this.splitContainerRight.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainerRight.Location = new System.Drawing.Point(0, 0);
            this.splitContainerRight.Name = "splitContainerRight";
            this.splitContainerRight.Orientation = System.Windows.Forms.Orientation.Horizontal;
            // 
            // splitContainerRight.Panel1
            // 
            this.splitContainerRight.Panel1.Controls.Add(this.tableOfContents);
            // 
            // splitContainerRight.Panel2
            // 
            this.splitContainerRight.Panel2.Controls.Add(this.loaderImageInfoTab);
            this.splitContainerRight.Size = new System.Drawing.Size(378, 583);
            this.splitContainerRight.SplitterDistance = 321;
            this.splitContainerRight.TabIndex = 0;
            // 
            // tableOfContents
            // 
            this.tableOfContents.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tableOfContents.Location = new System.Drawing.Point(0, 0);
            this.tableOfContents.Name = "tableOfContents";
            this.tableOfContents.Size = new System.Drawing.Size(378, 321);
            this.tableOfContents.TabIndex = 0;
            this.tableOfContents.Load += new System.EventHandler(this.tableOfContents_Load);
            // 
            // loaderImageInfoTab
            // 
            this.loaderImageInfoTab.Dock = System.Windows.Forms.DockStyle.Fill;
            this.loaderImageInfoTab.Image = global::Canal.Properties.Resources.loader;
            this.loaderImageInfoTab.Location = new System.Drawing.Point(0, 0);
            this.loaderImageInfoTab.Name = "loaderImageInfoTab";
            this.loaderImageInfoTab.Size = new System.Drawing.Size(378, 258);
            this.loaderImageInfoTab.SizeMode = System.Windows.Forms.PictureBoxSizeMode.CenterImage;
            this.loaderImageInfoTab.TabIndex = 5;
            this.loaderImageInfoTab.TabStop = false;
            // 
            // miniToolStrip
            // 
            this.miniToolStrip.AutoSize = false;
            this.miniToolStrip.CanOverflow = false;
            this.miniToolStrip.Dock = System.Windows.Forms.DockStyle.None;
            this.miniToolStrip.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
            this.miniToolStrip.Location = new System.Drawing.Point(131, 3);
            this.miniToolStrip.Name = "miniToolStrip";
            this.miniToolStrip.Size = new System.Drawing.Size(200, 25);
            this.miniToolStrip.TabIndex = 1;
            // 
            // FileControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.splitContainerTop);
            this.Margin = new System.Windows.Forms.Padding(2);
            this.Name = "FileControl";
            this.Size = new System.Drawing.Size(1061, 583);
            ((System.ComponentModel.ISupportInitialize)(this.codeBox)).EndInit();
            this.splitContainerTop.Panel1.ResumeLayout(false);
            this.splitContainerTop.Panel1.PerformLayout();
            this.splitContainerTop.Panel2.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.splitContainerTop)).EndInit();
            this.splitContainerTop.ResumeLayout(false);
            this.codeViewToolStrip.ResumeLayout(false);
            this.codeViewToolStrip.PerformLayout();
            this.splitContainerRight.Panel1.ResumeLayout(false);
            this.splitContainerRight.Panel2.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.splitContainerRight)).EndInit();
            this.splitContainerRight.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.loaderImageInfoTab)).EndInit();
            this.ResumeLayout(false);

        }

        #endregion
        private FastColoredTextBox codeBox;
        private System.Windows.Forms.SplitContainer splitContainerTop;
        private System.Windows.Forms.SplitContainer splitContainerRight;
        private System.Windows.Forms.ToolStrip codeViewToolStrip;
        private System.Windows.Forms.ToolStripTextBox searchBox;
        private System.Windows.Forms.ToolStripButton searchWithRegEx;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
        private ToolStrip miniToolStrip;
        private PictureBox loaderImageInfoTab;
        private ToolStripButton navigateBackward;
        private ToolStripSeparator toolStripSeparator2;
        private ToolStripButton navigateForwardButton;
        private ToolStripButton findPreviousButton;
        private ToolStripButton findNextButton;
        private ToolStripButton newButton;
        private ToolStripButton openButton;
        private ToolStripButton saveButton;
        private ToolStripSeparator toolStripSeparator4;
        private ToolStripButton undoButton;
        private ToolStripButton redoButton;
        private ToolStripSeparator toolStripSeparator3;
        private ToolStripButton formatCodeButton;
        private TableOfContents tableOfContents;
    }
}
