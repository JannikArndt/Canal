using System.Windows.Forms;
using FastColoredTextBoxNS;

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
            this.tocTreeView = new System.Windows.Forms.TreeView();
            this.structureTabControl = new System.Windows.Forms.TabControl();
            this.tocTabPage = new System.Windows.Forms.TabPage();
            this.loaderImageToc = new System.Windows.Forms.PictureBox();
            this.tocToolStrip = new System.Windows.Forms.ToolStrip();
            this.TocCopyButton = new System.Windows.Forms.ToolStripButton();
            this.TocCollapseAllButton = new System.Windows.Forms.ToolStripButton();
            this.TocExpandAllButton = new System.Windows.Forms.ToolStripButton();
            this.performsTabPage = new System.Windows.Forms.TabPage();
            this.loaderImagePerforms = new System.Windows.Forms.PictureBox();
            this.performsToolStrip = new System.Windows.Forms.ToolStrip();
            this.performsCopyButton = new System.Windows.Forms.ToolStripButton();
            this.performsCollapseAllButton = new System.Windows.Forms.ToolStripButton();
            this.performsExpandAllButton = new System.Windows.Forms.ToolStripButton();
            this.performsTreeView = new System.Windows.Forms.TreeView();
            this.variablesTabPage = new System.Windows.Forms.TabPage();
            this.variablesToolStrip = new System.Windows.Forms.ToolStrip();
            this.variablesCopyButton = new System.Windows.Forms.ToolStripButton();
            this.variablesCollapseAllButton = new System.Windows.Forms.ToolStripButton();
            this.variablesExpandAllButton = new System.Windows.Forms.ToolStripButton();
            this.variablesTreeView = new System.Windows.Forms.TreeView();
            this.proceduresTabPage = new System.Windows.Forms.TabPage();
            this.proceduresToolStrip = new System.Windows.Forms.ToolStrip();
            this.proceduresCopyButton = new System.Windows.Forms.ToolStripButton();
            this.proceduresCollapseAllButton = new System.Windows.Forms.ToolStripButton();
            this.proceduresExpandAllButton = new System.Windows.Forms.ToolStripButton();
            this.proceduresTreeView = new System.Windows.Forms.TreeView();
            this.filesTabPage = new System.Windows.Forms.TabPage();
            this.filesTabToolStrip = new System.Windows.Forms.ToolStrip();
            this.filesTabSearchBox = new System.Windows.Forms.ToolStripTextBox();
            this.fileTypeDropDownButton = new System.Windows.Forms.ToolStripDropDownButton();
            this.showFileTypes_cob = new System.Windows.Forms.ToolStripMenuItem();
            this.showFileTypes_txt = new System.Windows.Forms.ToolStripMenuItem();
            this.showFileTypes_src = new System.Windows.Forms.ToolStripMenuItem();
            this.showFileTypes_custom = new System.Windows.Forms.ToolStripTextBox();
            this.filesTreeView = new System.Windows.Forms.TreeView();
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
            this.loaderImageInfoTab = new System.Windows.Forms.PictureBox();
            this.miniToolStrip = new System.Windows.Forms.ToolStrip();
            this.structureTabControl.SuspendLayout();
            this.tocTabPage.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.loaderImageToc)).BeginInit();
            this.tocToolStrip.SuspendLayout();
            this.performsTabPage.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.loaderImagePerforms)).BeginInit();
            this.performsToolStrip.SuspendLayout();
            this.variablesTabPage.SuspendLayout();
            this.variablesToolStrip.SuspendLayout();
            this.proceduresTabPage.SuspendLayout();
            this.proceduresToolStrip.SuspendLayout();
            this.filesTabPage.SuspendLayout();
            this.filesTabToolStrip.SuspendLayout();
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
            // tocTreeView
            // 
            this.tocTreeView.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.tocTreeView.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.tocTreeView.Font = new System.Drawing.Font("Consolas", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.tocTreeView.ItemHeight = 22;
            this.tocTreeView.Location = new System.Drawing.Point(3, 30);
            this.tocTreeView.Margin = new System.Windows.Forms.Padding(2);
            this.tocTreeView.Name = "tocTreeView";
            this.tocTreeView.Size = new System.Drawing.Size(364, 262);
            this.tocTreeView.TabIndex = 1;
            this.tocTreeView.AfterSelect += new System.Windows.Forms.TreeViewEventHandler(this.TreeViewAfterSelect);
            // 
            // structureTabControl
            // 
            this.structureTabControl.Controls.Add(this.tocTabPage);
            this.structureTabControl.Controls.Add(this.performsTabPage);
            this.structureTabControl.Controls.Add(this.variablesTabPage);
            this.structureTabControl.Controls.Add(this.proceduresTabPage);
            this.structureTabControl.Controls.Add(this.filesTabPage);
            this.structureTabControl.Dock = System.Windows.Forms.DockStyle.Fill;
            this.structureTabControl.Location = new System.Drawing.Point(0, 0);
            this.structureTabControl.Name = "structureTabControl";
            this.structureTabControl.SelectedIndex = 0;
            this.structureTabControl.Size = new System.Drawing.Size(378, 321);
            this.structureTabControl.TabIndex = 5;
            // 
            // tocTabPage
            // 
            this.tocTabPage.Controls.Add(this.loaderImageToc);
            this.tocTabPage.Controls.Add(this.tocToolStrip);
            this.tocTabPage.Controls.Add(this.tocTreeView);
            this.tocTabPage.Location = new System.Drawing.Point(4, 22);
            this.tocTabPage.Name = "tocTabPage";
            this.tocTabPage.Padding = new System.Windows.Forms.Padding(3);
            this.tocTabPage.Size = new System.Drawing.Size(370, 295);
            this.tocTabPage.TabIndex = 0;
            this.tocTabPage.Text = "Table of Contents";
            this.tocTabPage.UseVisualStyleBackColor = true;
            // 
            // loaderImageToc
            // 
            this.loaderImageToc.Dock = System.Windows.Forms.DockStyle.Fill;
            this.loaderImageToc.Image = ((System.Drawing.Image)(resources.GetObject("loaderImageToc.Image")));
            this.loaderImageToc.Location = new System.Drawing.Point(3, 28);
            this.loaderImageToc.Name = "loaderImageToc";
            this.loaderImageToc.Size = new System.Drawing.Size(364, 264);
            this.loaderImageToc.SizeMode = System.Windows.Forms.PictureBoxSizeMode.CenterImage;
            this.loaderImageToc.TabIndex = 3;
            this.loaderImageToc.TabStop = false;
            // 
            // tocToolStrip
            // 
            this.tocToolStrip.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
            this.tocToolStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.TocCopyButton,
            this.TocCollapseAllButton,
            this.TocExpandAllButton});
            this.tocToolStrip.Location = new System.Drawing.Point(3, 3);
            this.tocToolStrip.Name = "tocToolStrip";
            this.tocToolStrip.Size = new System.Drawing.Size(364, 25);
            this.tocToolStrip.TabIndex = 2;
            this.tocToolStrip.Text = "toolStrip1";
            // 
            // TocCopyButton
            // 
            this.TocCopyButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.TocCopyButton.Image = ((System.Drawing.Image)(resources.GetObject("TocCopyButton.Image")));
            this.TocCopyButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.TocCopyButton.Name = "TocCopyButton";
            this.TocCopyButton.Size = new System.Drawing.Size(23, 22);
            this.TocCopyButton.Text = "Copy";
            this.TocCopyButton.Click += new System.EventHandler(this.ExportTocClick);
            // 
            // TocCollapseAllButton
            // 
            this.TocCollapseAllButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.TocCollapseAllButton.Image = ((System.Drawing.Image)(resources.GetObject("TocCollapseAllButton.Image")));
            this.TocCollapseAllButton.ImageTransparentColor = System.Drawing.Color.Black;
            this.TocCollapseAllButton.Name = "TocCollapseAllButton";
            this.TocCollapseAllButton.Size = new System.Drawing.Size(23, 22);
            this.TocCollapseAllButton.Text = "Collapse All";
            this.TocCollapseAllButton.Click += new System.EventHandler(this.TocCollapseAllButtonClick);
            // 
            // TocExpandAllButton
            // 
            this.TocExpandAllButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.TocExpandAllButton.Image = ((System.Drawing.Image)(resources.GetObject("TocExpandAllButton.Image")));
            this.TocExpandAllButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.TocExpandAllButton.Name = "TocExpandAllButton";
            this.TocExpandAllButton.Size = new System.Drawing.Size(23, 22);
            this.TocExpandAllButton.Text = "Expand all";
            this.TocExpandAllButton.Click += new System.EventHandler(this.TocExpandAllButtonClick);
            // 
            // performsTabPage
            // 
            this.performsTabPage.Controls.Add(this.loaderImagePerforms);
            this.performsTabPage.Controls.Add(this.performsToolStrip);
            this.performsTabPage.Controls.Add(this.performsTreeView);
            this.performsTabPage.Location = new System.Drawing.Point(4, 22);
            this.performsTabPage.Name = "performsTabPage";
            this.performsTabPage.Padding = new System.Windows.Forms.Padding(3);
            this.performsTabPage.Size = new System.Drawing.Size(370, 295);
            this.performsTabPage.TabIndex = 1;
            this.performsTabPage.Text = "Performs";
            this.performsTabPage.UseVisualStyleBackColor = true;
            // 
            // loaderImagePerforms
            // 
            this.loaderImagePerforms.Dock = System.Windows.Forms.DockStyle.Fill;
            this.loaderImagePerforms.Image = ((System.Drawing.Image)(resources.GetObject("loaderImagePerforms.Image")));
            this.loaderImagePerforms.Location = new System.Drawing.Point(3, 28);
            this.loaderImagePerforms.Name = "loaderImagePerforms";
            this.loaderImagePerforms.Size = new System.Drawing.Size(364, 264);
            this.loaderImagePerforms.SizeMode = System.Windows.Forms.PictureBoxSizeMode.CenterImage;
            this.loaderImagePerforms.TabIndex = 4;
            this.loaderImagePerforms.TabStop = false;
            // 
            // performsToolStrip
            // 
            this.performsToolStrip.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
            this.performsToolStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.performsCopyButton,
            this.performsCollapseAllButton,
            this.performsExpandAllButton});
            this.performsToolStrip.Location = new System.Drawing.Point(3, 3);
            this.performsToolStrip.Name = "performsToolStrip";
            this.performsToolStrip.Size = new System.Drawing.Size(364, 25);
            this.performsToolStrip.TabIndex = 3;
            this.performsToolStrip.Text = "toolStrip1";
            // 
            // performsCopyButton
            // 
            this.performsCopyButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.performsCopyButton.Image = ((System.Drawing.Image)(resources.GetObject("performsCopyButton.Image")));
            this.performsCopyButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.performsCopyButton.Name = "performsCopyButton";
            this.performsCopyButton.Size = new System.Drawing.Size(23, 22);
            this.performsCopyButton.Text = "Copy";
            this.performsCopyButton.Click += new System.EventHandler(this.PerformsCopyButtonClick);
            // 
            // performsCollapseAllButton
            // 
            this.performsCollapseAllButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.performsCollapseAllButton.Image = ((System.Drawing.Image)(resources.GetObject("performsCollapseAllButton.Image")));
            this.performsCollapseAllButton.ImageTransparentColor = System.Drawing.Color.Black;
            this.performsCollapseAllButton.Name = "performsCollapseAllButton";
            this.performsCollapseAllButton.Size = new System.Drawing.Size(23, 22);
            this.performsCollapseAllButton.Text = "Collapse All";
            this.performsCollapseAllButton.Click += new System.EventHandler(this.PerformsCollapseAllButtonClick);
            // 
            // performsExpandAllButton
            // 
            this.performsExpandAllButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.performsExpandAllButton.Image = ((System.Drawing.Image)(resources.GetObject("performsExpandAllButton.Image")));
            this.performsExpandAllButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.performsExpandAllButton.Name = "performsExpandAllButton";
            this.performsExpandAllButton.Size = new System.Drawing.Size(23, 22);
            this.performsExpandAllButton.Text = "Expand all";
            this.performsExpandAllButton.Click += new System.EventHandler(this.PerformsExpandAllButtonClick);
            // 
            // performsTreeView
            // 
            this.performsTreeView.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.performsTreeView.Font = new System.Drawing.Font("Consolas", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.performsTreeView.ItemHeight = 22;
            this.performsTreeView.Location = new System.Drawing.Point(3, 31);
            this.performsTreeView.Name = "performsTreeView";
            this.performsTreeView.Size = new System.Drawing.Size(364, 261);
            this.performsTreeView.TabIndex = 0;
            this.performsTreeView.AfterSelect += new System.Windows.Forms.TreeViewEventHandler(this.PerformsTreeAfterSelect);
            // 
            // variablesTabPage
            // 
            this.variablesTabPage.Controls.Add(this.variablesToolStrip);
            this.variablesTabPage.Controls.Add(this.variablesTreeView);
            this.variablesTabPage.Location = new System.Drawing.Point(4, 22);
            this.variablesTabPage.Name = "variablesTabPage";
            this.variablesTabPage.Padding = new System.Windows.Forms.Padding(3);
            this.variablesTabPage.Size = new System.Drawing.Size(370, 295);
            this.variablesTabPage.TabIndex = 2;
            this.variablesTabPage.Text = "Variables";
            this.variablesTabPage.UseVisualStyleBackColor = true;
            // 
            // variablesToolStrip
            // 
            this.variablesToolStrip.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
            this.variablesToolStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.variablesCopyButton,
            this.variablesCollapseAllButton,
            this.variablesExpandAllButton});
            this.variablesToolStrip.Location = new System.Drawing.Point(3, 3);
            this.variablesToolStrip.Name = "variablesToolStrip";
            this.variablesToolStrip.Size = new System.Drawing.Size(364, 25);
            this.variablesToolStrip.TabIndex = 3;
            this.variablesToolStrip.Text = "toolStrip1";
            // 
            // variablesCopyButton
            // 
            this.variablesCopyButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.variablesCopyButton.Image = ((System.Drawing.Image)(resources.GetObject("variablesCopyButton.Image")));
            this.variablesCopyButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.variablesCopyButton.Name = "variablesCopyButton";
            this.variablesCopyButton.Size = new System.Drawing.Size(23, 22);
            this.variablesCopyButton.Text = "Copy";
            this.variablesCopyButton.Click += new System.EventHandler(this.VariablesCopyButtonClick);
            // 
            // variablesCollapseAllButton
            // 
            this.variablesCollapseAllButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.variablesCollapseAllButton.Image = ((System.Drawing.Image)(resources.GetObject("variablesCollapseAllButton.Image")));
            this.variablesCollapseAllButton.ImageTransparentColor = System.Drawing.Color.Black;
            this.variablesCollapseAllButton.Name = "variablesCollapseAllButton";
            this.variablesCollapseAllButton.Size = new System.Drawing.Size(23, 22);
            this.variablesCollapseAllButton.Text = "Collapse All";
            this.variablesCollapseAllButton.Click += new System.EventHandler(this.VariablesCollapseAllButtonClick);
            // 
            // variablesExpandAllButton
            // 
            this.variablesExpandAllButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.variablesExpandAllButton.Image = ((System.Drawing.Image)(resources.GetObject("variablesExpandAllButton.Image")));
            this.variablesExpandAllButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.variablesExpandAllButton.Name = "variablesExpandAllButton";
            this.variablesExpandAllButton.Size = new System.Drawing.Size(23, 22);
            this.variablesExpandAllButton.Text = "Expand all";
            this.variablesExpandAllButton.Click += new System.EventHandler(this.VariablesExpandAllButtonClick);
            // 
            // variablesTreeView
            // 
            this.variablesTreeView.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.variablesTreeView.Font = new System.Drawing.Font("Consolas", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.variablesTreeView.ItemHeight = 18;
            this.variablesTreeView.Location = new System.Drawing.Point(3, 31);
            this.variablesTreeView.Name = "variablesTreeView";
            this.variablesTreeView.Size = new System.Drawing.Size(364, 261);
            this.variablesTreeView.TabIndex = 0;
            this.variablesTreeView.AfterSelect += new System.Windows.Forms.TreeViewEventHandler(this.VariablesTreeViewAfterSelect);
            // 
            // proceduresTabPage
            // 
            this.proceduresTabPage.Controls.Add(this.proceduresToolStrip);
            this.proceduresTabPage.Controls.Add(this.proceduresTreeView);
            this.proceduresTabPage.Location = new System.Drawing.Point(4, 22);
            this.proceduresTabPage.Name = "proceduresTabPage";
            this.proceduresTabPage.Padding = new System.Windows.Forms.Padding(3);
            this.proceduresTabPage.Size = new System.Drawing.Size(370, 295);
            this.proceduresTabPage.TabIndex = 3;
            this.proceduresTabPage.Text = "Procedures";
            this.proceduresTabPage.UseVisualStyleBackColor = true;
            // 
            // proceduresToolStrip
            // 
            this.proceduresToolStrip.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
            this.proceduresToolStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.proceduresCopyButton,
            this.proceduresCollapseAllButton,
            this.proceduresExpandAllButton});
            this.proceduresToolStrip.Location = new System.Drawing.Point(3, 3);
            this.proceduresToolStrip.Name = "proceduresToolStrip";
            this.proceduresToolStrip.Size = new System.Drawing.Size(364, 25);
            this.proceduresToolStrip.TabIndex = 1;
            this.proceduresToolStrip.Text = "toolStrip1";
            // 
            // proceduresCopyButton
            // 
            this.proceduresCopyButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.proceduresCopyButton.Image = ((System.Drawing.Image)(resources.GetObject("proceduresCopyButton.Image")));
            this.proceduresCopyButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.proceduresCopyButton.Name = "proceduresCopyButton";
            this.proceduresCopyButton.Size = new System.Drawing.Size(23, 22);
            this.proceduresCopyButton.Text = "Copy";
            this.proceduresCopyButton.Click += new System.EventHandler(this.CopyProceduresClick);
            // 
            // proceduresCollapseAllButton
            // 
            this.proceduresCollapseAllButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.proceduresCollapseAllButton.Image = ((System.Drawing.Image)(resources.GetObject("proceduresCollapseAllButton.Image")));
            this.proceduresCollapseAllButton.ImageTransparentColor = System.Drawing.Color.Black;
            this.proceduresCollapseAllButton.Name = "proceduresCollapseAllButton";
            this.proceduresCollapseAllButton.Size = new System.Drawing.Size(23, 22);
            this.proceduresCollapseAllButton.Text = "Collapse All";
            this.proceduresCollapseAllButton.Click += new System.EventHandler(this.ProceduresCollapseAllButtonClick);
            // 
            // proceduresExpandAllButton
            // 
            this.proceduresExpandAllButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.proceduresExpandAllButton.Image = ((System.Drawing.Image)(resources.GetObject("proceduresExpandAllButton.Image")));
            this.proceduresExpandAllButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.proceduresExpandAllButton.Name = "proceduresExpandAllButton";
            this.proceduresExpandAllButton.Size = new System.Drawing.Size(23, 22);
            this.proceduresExpandAllButton.Text = "Expand all";
            this.proceduresExpandAllButton.Click += new System.EventHandler(this.ProceduresExpandAllButtonClick);
            // 
            // proceduresTreeView
            // 
            this.proceduresTreeView.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.proceduresTreeView.Font = new System.Drawing.Font("Consolas", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.proceduresTreeView.ItemHeight = 22;
            this.proceduresTreeView.Location = new System.Drawing.Point(3, 31);
            this.proceduresTreeView.Name = "proceduresTreeView";
            this.proceduresTreeView.Size = new System.Drawing.Size(364, 261);
            this.proceduresTreeView.TabIndex = 0;
            this.proceduresTreeView.AfterSelect += new System.Windows.Forms.TreeViewEventHandler(this.ProceduresTreeViewAfterSelect);
            // 
            // filesTabPage
            // 
            this.filesTabPage.Controls.Add(this.filesTabToolStrip);
            this.filesTabPage.Controls.Add(this.filesTreeView);
            this.filesTabPage.Location = new System.Drawing.Point(4, 22);
            this.filesTabPage.Name = "filesTabPage";
            this.filesTabPage.Padding = new System.Windows.Forms.Padding(3);
            this.filesTabPage.Size = new System.Drawing.Size(370, 295);
            this.filesTabPage.TabIndex = 4;
            this.filesTabPage.Text = "Files";
            this.filesTabPage.UseVisualStyleBackColor = true;
            // 
            // filesTabToolStrip
            // 
            this.filesTabToolStrip.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
            this.filesTabToolStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.filesTabSearchBox,
            this.fileTypeDropDownButton});
            this.filesTabToolStrip.Location = new System.Drawing.Point(3, 3);
            this.filesTabToolStrip.Name = "filesTabToolStrip";
            this.filesTabToolStrip.Size = new System.Drawing.Size(364, 25);
            this.filesTabToolStrip.TabIndex = 1;
            this.filesTabToolStrip.Text = "toolStrip1";
            // 
            // filesTabSearchBox
            // 
            this.filesTabSearchBox.Name = "filesTabSearchBox";
            this.filesTabSearchBox.Size = new System.Drawing.Size(100, 25);
            this.filesTabSearchBox.Text = "Search Files...";
            this.filesTabSearchBox.Enter += new System.EventHandler(this.SearchBoxEnter);
            this.filesTabSearchBox.Leave += new System.EventHandler(this.SearchBoxLeave);
            this.filesTabSearchBox.TextChanged += new System.EventHandler(this.FilesTabSearchBoxTextChanged);
            // 
            // fileTypeDropDownButton
            // 
            this.fileTypeDropDownButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.fileTypeDropDownButton.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.showFileTypes_cob,
            this.showFileTypes_txt,
            this.showFileTypes_src,
            this.showFileTypes_custom});
            this.fileTypeDropDownButton.Image = ((System.Drawing.Image)(resources.GetObject("fileTypeDropDownButton.Image")));
            this.fileTypeDropDownButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.fileTypeDropDownButton.Name = "fileTypeDropDownButton";
            this.fileTypeDropDownButton.Size = new System.Drawing.Size(29, 22);
            this.fileTypeDropDownButton.Text = "File Types";
            this.fileTypeDropDownButton.Click += new System.EventHandler(this.RefreshUsedFileTypes);
            // 
            // showFileTypes_cob
            // 
            this.showFileTypes_cob.Checked = true;
            this.showFileTypes_cob.CheckOnClick = true;
            this.showFileTypes_cob.CheckState = System.Windows.Forms.CheckState.Checked;
            this.showFileTypes_cob.Name = "showFileTypes_cob";
            this.showFileTypes_cob.Size = new System.Drawing.Size(160, 22);
            this.showFileTypes_cob.Text = "*.cob/*.cbl";
            this.showFileTypes_cob.Click += new System.EventHandler(this.SettingsSourceCodeFilesClick);
            // 
            // showFileTypes_txt
            // 
            this.showFileTypes_txt.CheckOnClick = true;
            this.showFileTypes_txt.Name = "showFileTypes_txt";
            this.showFileTypes_txt.Size = new System.Drawing.Size(160, 22);
            this.showFileTypes_txt.Text = "*.txt";
            this.showFileTypes_txt.Click += new System.EventHandler(this.SettingsSourceCodeFilesClick);
            // 
            // showFileTypes_src
            // 
            this.showFileTypes_src.CheckOnClick = true;
            this.showFileTypes_src.Name = "showFileTypes_src";
            this.showFileTypes_src.Size = new System.Drawing.Size(160, 22);
            this.showFileTypes_src.Text = "*.src";
            this.showFileTypes_src.Click += new System.EventHandler(this.SettingsSourceCodeFilesClick);
            // 
            // showFileTypes_custom
            // 
            this.showFileTypes_custom.Name = "showFileTypes_custom";
            this.showFileTypes_custom.Size = new System.Drawing.Size(100, 23);
            // 
            // filesTreeView
            // 
            this.filesTreeView.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.filesTreeView.Location = new System.Drawing.Point(0, 31);
            this.filesTreeView.Name = "filesTreeView";
            this.filesTreeView.Size = new System.Drawing.Size(370, 261);
            this.filesTreeView.TabIndex = 0;
            this.filesTreeView.DoubleClick += new System.EventHandler(this.FilesTreeViewDoubleClick);
            this.filesTreeView.KeyUp += new System.Windows.Forms.KeyEventHandler(this.FilesTreeViewKeyUp);
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
            this.codeBox.AutoScrollMinSize = new System.Drawing.Size(25, 15);
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
            this.splitContainerRight.Panel1.Controls.Add(this.structureTabControl);
            // 
            // splitContainerRight.Panel2
            // 
            this.splitContainerRight.Panel2.Controls.Add(this.loaderImageInfoTab);
            this.splitContainerRight.Size = new System.Drawing.Size(378, 583);
            this.splitContainerRight.SplitterDistance = 321;
            this.splitContainerRight.TabIndex = 0;
            // 
            // loaderImageInfoTab
            // 
            this.loaderImageInfoTab.Dock = System.Windows.Forms.DockStyle.Fill;
            this.loaderImageInfoTab.Image = ((System.Drawing.Image)(resources.GetObject("loaderImageInfoTab.Image")));
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
            this.structureTabControl.ResumeLayout(false);
            this.tocTabPage.ResumeLayout(false);
            this.tocTabPage.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.loaderImageToc)).EndInit();
            this.tocToolStrip.ResumeLayout(false);
            this.tocToolStrip.PerformLayout();
            this.performsTabPage.ResumeLayout(false);
            this.performsTabPage.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.loaderImagePerforms)).EndInit();
            this.performsToolStrip.ResumeLayout(false);
            this.performsToolStrip.PerformLayout();
            this.variablesTabPage.ResumeLayout(false);
            this.variablesTabPage.PerformLayout();
            this.variablesToolStrip.ResumeLayout(false);
            this.variablesToolStrip.PerformLayout();
            this.proceduresTabPage.ResumeLayout(false);
            this.proceduresTabPage.PerformLayout();
            this.proceduresToolStrip.ResumeLayout(false);
            this.proceduresToolStrip.PerformLayout();
            this.filesTabPage.ResumeLayout(false);
            this.filesTabPage.PerformLayout();
            this.filesTabToolStrip.ResumeLayout(false);
            this.filesTabToolStrip.PerformLayout();
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
        private System.Windows.Forms.TreeView tocTreeView;
        private FastColoredTextBox codeBox;
        private System.Windows.Forms.TabControl structureTabControl;
        private System.Windows.Forms.TabPage tocTabPage;
        private System.Windows.Forms.TabPage performsTabPage;
        private System.Windows.Forms.TreeView performsTreeView;
        private System.Windows.Forms.TabPage variablesTabPage;
        private System.Windows.Forms.TreeView variablesTreeView;
        private System.Windows.Forms.TabPage proceduresTabPage;
        private System.Windows.Forms.TreeView proceduresTreeView;
        private System.Windows.Forms.ToolStrip tocToolStrip;
        private System.Windows.Forms.ToolStripButton TocCopyButton;
        private System.Windows.Forms.ToolStrip proceduresToolStrip;
        private System.Windows.Forms.ToolStripButton proceduresCopyButton;
        private System.Windows.Forms.ToolStripButton TocExpandAllButton;
        private System.Windows.Forms.ToolStripButton TocCollapseAllButton;
        private System.Windows.Forms.ToolStrip performsToolStrip;
        private System.Windows.Forms.ToolStripButton performsCopyButton;
        private System.Windows.Forms.ToolStripButton performsExpandAllButton;
        private System.Windows.Forms.ToolStripButton performsCollapseAllButton;
        private System.Windows.Forms.ToolStrip variablesToolStrip;
        private System.Windows.Forms.ToolStripButton variablesCopyButton;
        private System.Windows.Forms.ToolStripButton variablesExpandAllButton;
        private System.Windows.Forms.ToolStripButton variablesCollapseAllButton;
        private System.Windows.Forms.ToolStripButton proceduresExpandAllButton;
        private System.Windows.Forms.ToolStripButton proceduresCollapseAllButton;
        private System.Windows.Forms.SplitContainer splitContainerTop;
        private System.Windows.Forms.SplitContainer splitContainerRight;
        private System.Windows.Forms.ToolStrip codeViewToolStrip;
        private System.Windows.Forms.ToolStripTextBox searchBox;
        private System.Windows.Forms.ToolStripButton searchWithRegEx;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
        private TabPage filesTabPage;
        private ToolStrip filesTabToolStrip;
        private ToolStripTextBox filesTabSearchBox;
        private ToolStripDropDownButton fileTypeDropDownButton;
        private ToolStripMenuItem showFileTypes_cob;
        private ToolStripMenuItem showFileTypes_txt;
        private ToolStripMenuItem showFileTypes_src;
        private ToolStripTextBox showFileTypes_custom;
        private TreeView filesTreeView;
        private ToolStrip miniToolStrip;
        private PictureBox loaderImageInfoTab;
        private PictureBox loaderImageToc;
        private ToolStripButton navigateBackward;
        private ToolStripSeparator toolStripSeparator2;
        private ToolStripButton navigateForwardButton;
        private PictureBox loaderImagePerforms;
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
    }
}
