﻿using FastColoredTextBoxNS;
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
            this.treeView = new System.Windows.Forms.TreeView();
            this.structureTabControl = new System.Windows.Forms.TabControl();
            this.tocTabPage = new System.Windows.Forms.TabPage();
            this.loaderImageToc = new System.Windows.Forms.PictureBox();
            this.tocToolStrip = new System.Windows.Forms.ToolStrip();
            this.TocCopyButton = new System.Windows.Forms.ToolStripButton();
            this.TocCollapseAllButton = new System.Windows.Forms.ToolStripButton();
            this.TocExpandAllButton = new System.Windows.Forms.ToolStripButton();
            this.performsTabPage = new System.Windows.Forms.TabPage();
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
            this.codeBox = new FastColoredTextBoxNS.FastColoredTextBox();
            this.splitContainerTop = new System.Windows.Forms.SplitContainer();
            this.codeViewToolStrip = new System.Windows.Forms.ToolStrip();
            this.searchBox = new System.Windows.Forms.ToolStripTextBox();
            this.searchWithRegEx = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.ResolveCopysButton = new System.Windows.Forms.ToolStripButton();
            this.toolStripProgressBar = new System.Windows.Forms.ToolStripProgressBar();
            this.splitContainerRight = new System.Windows.Forms.SplitContainer();
            this.infoTabControl = new System.Windows.Forms.TabControl();
            this.infoTabPage = new System.Windows.Forms.TabPage();
            this.loaderImageInfoTab = new System.Windows.Forms.PictureBox();
            this.filesTabPage = new System.Windows.Forms.TabPage();
            this.filesTabToolStrip = new System.Windows.Forms.ToolStrip();
            this.filesTabSearchBox = new System.Windows.Forms.ToolStripTextBox();
            this.fileTypeDropDownButton = new System.Windows.Forms.ToolStripDropDownButton();
            this.showFileTypes_cob = new System.Windows.Forms.ToolStripMenuItem();
            this.showFileTypes_txt = new System.Windows.Forms.ToolStripMenuItem();
            this.showFileTypes_src = new System.Windows.Forms.ToolStripMenuItem();
            this.showFileTypes_custom = new System.Windows.Forms.ToolStripTextBox();
            this.filesTreeView = new System.Windows.Forms.TreeView();
            this.structureTabControl.SuspendLayout();
            this.tocTabPage.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.loaderImageToc)).BeginInit();
            this.tocToolStrip.SuspendLayout();
            this.performsTabPage.SuspendLayout();
            this.performsToolStrip.SuspendLayout();
            this.variablesTabPage.SuspendLayout();
            this.variablesToolStrip.SuspendLayout();
            this.proceduresTabPage.SuspendLayout();
            this.proceduresToolStrip.SuspendLayout();
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
            this.infoTabControl.SuspendLayout();
            this.infoTabPage.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.loaderImageInfoTab)).BeginInit();
            this.filesTabPage.SuspendLayout();
            this.filesTabToolStrip.SuspendLayout();
            this.SuspendLayout();
            // 
            // treeView
            // 
            this.treeView.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.treeView.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.treeView.Font = new System.Drawing.Font("Consolas", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.treeView.ItemHeight = 22;
            this.treeView.Location = new System.Drawing.Point(3, 30);
            this.treeView.Margin = new System.Windows.Forms.Padding(2);
            this.treeView.Name = "treeView";
            this.treeView.Size = new System.Drawing.Size(257, 524);
            this.treeView.TabIndex = 1;
            this.treeView.AfterSelect += new System.Windows.Forms.TreeViewEventHandler(this.treeView_AfterSelect);
            // 
            // structureTabControl
            // 
            this.structureTabControl.Controls.Add(this.tocTabPage);
            this.structureTabControl.Controls.Add(this.performsTabPage);
            this.structureTabControl.Controls.Add(this.variablesTabPage);
            this.structureTabControl.Controls.Add(this.proceduresTabPage);
            this.structureTabControl.Dock = System.Windows.Forms.DockStyle.Fill;
            this.structureTabControl.Location = new System.Drawing.Point(0, 0);
            this.structureTabControl.Name = "structureTabControl";
            this.structureTabControl.SelectedIndex = 0;
            this.structureTabControl.Size = new System.Drawing.Size(271, 583);
            this.structureTabControl.TabIndex = 5;
            // 
            // tocTabPage
            // 
            this.tocTabPage.Controls.Add(this.loaderImageToc);
            this.tocTabPage.Controls.Add(this.tocToolStrip);
            this.tocTabPage.Controls.Add(this.treeView);
            this.tocTabPage.Location = new System.Drawing.Point(4, 22);
            this.tocTabPage.Name = "tocTabPage";
            this.tocTabPage.Padding = new System.Windows.Forms.Padding(3);
            this.tocTabPage.Size = new System.Drawing.Size(263, 557);
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
            this.loaderImageToc.Size = new System.Drawing.Size(257, 526);
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
            this.tocToolStrip.Size = new System.Drawing.Size(257, 25);
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
            this.TocCollapseAllButton.Click += new System.EventHandler(this.TocCollapseAllButton_Click);
            // 
            // TocExpandAllButton
            // 
            this.TocExpandAllButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.TocExpandAllButton.Image = ((System.Drawing.Image)(resources.GetObject("TocExpandAllButton.Image")));
            this.TocExpandAllButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.TocExpandAllButton.Name = "TocExpandAllButton";
            this.TocExpandAllButton.Size = new System.Drawing.Size(23, 22);
            this.TocExpandAllButton.Text = "Expand all";
            this.TocExpandAllButton.Click += new System.EventHandler(this.TocExpandAllButton_Click);
            // 
            // performsTabPage
            // 
            this.performsTabPage.Controls.Add(this.performsToolStrip);
            this.performsTabPage.Controls.Add(this.performsTreeView);
            this.performsTabPage.Location = new System.Drawing.Point(4, 22);
            this.performsTabPage.Name = "performsTabPage";
            this.performsTabPage.Padding = new System.Windows.Forms.Padding(3);
            this.performsTabPage.Size = new System.Drawing.Size(263, 557);
            this.performsTabPage.TabIndex = 1;
            this.performsTabPage.Text = "Performs";
            this.performsTabPage.UseVisualStyleBackColor = true;
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
            this.performsToolStrip.Size = new System.Drawing.Size(257, 25);
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
            this.performsCopyButton.Click += new System.EventHandler(this.performsCopyButton_Click);
            // 
            // performsCollapseAllButton
            // 
            this.performsCollapseAllButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.performsCollapseAllButton.Image = ((System.Drawing.Image)(resources.GetObject("performsCollapseAllButton.Image")));
            this.performsCollapseAllButton.ImageTransparentColor = System.Drawing.Color.Black;
            this.performsCollapseAllButton.Name = "performsCollapseAllButton";
            this.performsCollapseAllButton.Size = new System.Drawing.Size(23, 22);
            this.performsCollapseAllButton.Text = "Collapse All";
            this.performsCollapseAllButton.Click += new System.EventHandler(this.performsCollapseAllButton_Click);
            // 
            // performsExpandAllButton
            // 
            this.performsExpandAllButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.performsExpandAllButton.Image = ((System.Drawing.Image)(resources.GetObject("performsExpandAllButton.Image")));
            this.performsExpandAllButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.performsExpandAllButton.Name = "performsExpandAllButton";
            this.performsExpandAllButton.Size = new System.Drawing.Size(23, 22);
            this.performsExpandAllButton.Text = "Expand all";
            this.performsExpandAllButton.Click += new System.EventHandler(this.performsExpandAllButton_Click);
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
            this.performsTreeView.Size = new System.Drawing.Size(257, 523);
            this.performsTreeView.TabIndex = 0;
            this.performsTreeView.AfterSelect += new System.Windows.Forms.TreeViewEventHandler(this.performsTree_AfterSelect);
            // 
            // variablesTabPage
            // 
            this.variablesTabPage.Controls.Add(this.variablesToolStrip);
            this.variablesTabPage.Controls.Add(this.variablesTreeView);
            this.variablesTabPage.Location = new System.Drawing.Point(4, 22);
            this.variablesTabPage.Name = "variablesTabPage";
            this.variablesTabPage.Padding = new System.Windows.Forms.Padding(3);
            this.variablesTabPage.Size = new System.Drawing.Size(263, 557);
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
            this.variablesToolStrip.Size = new System.Drawing.Size(257, 25);
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
            this.variablesCopyButton.Click += new System.EventHandler(this.variablesCopyButton_Click);
            // 
            // variablesCollapseAllButton
            // 
            this.variablesCollapseAllButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.variablesCollapseAllButton.Image = ((System.Drawing.Image)(resources.GetObject("variablesCollapseAllButton.Image")));
            this.variablesCollapseAllButton.ImageTransparentColor = System.Drawing.Color.Black;
            this.variablesCollapseAllButton.Name = "variablesCollapseAllButton";
            this.variablesCollapseAllButton.Size = new System.Drawing.Size(23, 22);
            this.variablesCollapseAllButton.Text = "Collapse All";
            this.variablesCollapseAllButton.Click += new System.EventHandler(this.variablesCollapseAllButton_Click);
            // 
            // variablesExpandAllButton
            // 
            this.variablesExpandAllButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.variablesExpandAllButton.Image = ((System.Drawing.Image)(resources.GetObject("variablesExpandAllButton.Image")));
            this.variablesExpandAllButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.variablesExpandAllButton.Name = "variablesExpandAllButton";
            this.variablesExpandAllButton.Size = new System.Drawing.Size(23, 22);
            this.variablesExpandAllButton.Text = "Expand all";
            this.variablesExpandAllButton.Click += new System.EventHandler(this.variablesExpandAllButton_Click);
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
            this.variablesTreeView.Size = new System.Drawing.Size(257, 523);
            this.variablesTreeView.TabIndex = 0;
            this.variablesTreeView.AfterSelect += new System.Windows.Forms.TreeViewEventHandler(this.variablesTreeView_AfterSelect);
            // 
            // proceduresTabPage
            // 
            this.proceduresTabPage.Controls.Add(this.proceduresToolStrip);
            this.proceduresTabPage.Controls.Add(this.proceduresTreeView);
            this.proceduresTabPage.Location = new System.Drawing.Point(4, 22);
            this.proceduresTabPage.Name = "proceduresTabPage";
            this.proceduresTabPage.Padding = new System.Windows.Forms.Padding(3);
            this.proceduresTabPage.Size = new System.Drawing.Size(263, 557);
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
            this.proceduresToolStrip.Size = new System.Drawing.Size(257, 25);
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
            this.proceduresCollapseAllButton.Click += new System.EventHandler(this.proceduresCollapseAllButton_Click);
            // 
            // proceduresExpandAllButton
            // 
            this.proceduresExpandAllButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.proceduresExpandAllButton.Image = ((System.Drawing.Image)(resources.GetObject("proceduresExpandAllButton.Image")));
            this.proceduresExpandAllButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.proceduresExpandAllButton.Name = "proceduresExpandAllButton";
            this.proceduresExpandAllButton.Size = new System.Drawing.Size(23, 22);
            this.proceduresExpandAllButton.Text = "Expand all";
            this.proceduresExpandAllButton.Click += new System.EventHandler(this.proceduresExpandAllButton_Click);
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
            this.proceduresTreeView.Size = new System.Drawing.Size(257, 523);
            this.proceduresTreeView.TabIndex = 0;
            this.proceduresTreeView.AfterSelect += new System.Windows.Forms.TreeViewEventHandler(this.proceduresTreeView_AfterSelect);
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
            this.codeBox.AutoScrollMinSize = new System.Drawing.Size(25, 15);
            this.codeBox.BackBrush = null;
            this.codeBox.CharHeight = 15;
            this.codeBox.CharWidth = 7;
            this.codeBox.Cursor = System.Windows.Forms.Cursors.IBeam;
            this.codeBox.DisabledColor = System.Drawing.Color.FromArgb(((int)(((byte)(100)))), ((int)(((byte)(180)))), ((int)(((byte)(180)))), ((int)(((byte)(180)))));
            this.codeBox.Font = new System.Drawing.Font("Consolas", 9.75F);
            this.codeBox.Hotkeys = resources.GetString("codeBox.Hotkeys");
            this.codeBox.IsReplaceMode = false;
            this.codeBox.Location = new System.Drawing.Point(0, 27);
            this.codeBox.Margin = new System.Windows.Forms.Padding(2);
            this.codeBox.Name = "codeBox";
            this.codeBox.Paddings = new System.Windows.Forms.Padding(0);
            this.codeBox.SelectionColor = System.Drawing.Color.FromArgb(((int)(((byte)(60)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(255)))));
            this.codeBox.ServiceColors = ((FastColoredTextBoxNS.ServiceColors)(resources.GetObject("codeBox.ServiceColors")));
            this.codeBox.Size = new System.Drawing.Size(680, 556);
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
            this.splitContainerTop.Size = new System.Drawing.Size(1173, 583);
            this.splitContainerTop.SplitterDistance = 680;
            this.splitContainerTop.TabIndex = 6;
            // 
            // codeViewToolStrip
            // 
            this.codeViewToolStrip.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
            this.codeViewToolStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.searchBox,
            this.searchWithRegEx,
            this.toolStripSeparator1,
            this.ResolveCopysButton,
            this.toolStripProgressBar});
            this.codeViewToolStrip.Location = new System.Drawing.Point(0, 0);
            this.codeViewToolStrip.Name = "codeViewToolStrip";
            this.codeViewToolStrip.Size = new System.Drawing.Size(680, 25);
            this.codeViewToolStrip.TabIndex = 1;
            this.codeViewToolStrip.Text = "codeViewToolStrip";
            // 
            // searchBox
            // 
            this.searchBox.Name = "searchBox";
            this.searchBox.Size = new System.Drawing.Size(200, 25);
            this.searchBox.Text = "Search...";
            this.searchBox.Enter += new System.EventHandler(this.searchBox_Enter);
            this.searchBox.Leave += new System.EventHandler(this.searchBox_Leave);
            this.searchBox.KeyDown += new System.Windows.Forms.KeyEventHandler(this.searchBox_KeyDown);
            this.searchBox.TextChanged += new System.EventHandler(this.seachBox_TextChanged);
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
            // ResolveCopysButton
            // 
            this.ResolveCopysButton.Image = ((System.Drawing.Image)(resources.GetObject("ResolveCopysButton.Image")));
            this.ResolveCopysButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.ResolveCopysButton.Name = "ResolveCopysButton";
            this.ResolveCopysButton.Size = new System.Drawing.Size(106, 22);
            this.ResolveCopysButton.Text = "Resolve COPYs";
            this.ResolveCopysButton.Click += new System.EventHandler(this.ResolveCopysButton_Click);
            // 
            // toolStripProgressBar
            // 
            this.toolStripProgressBar.Name = "toolStripProgressBar";
            this.toolStripProgressBar.Size = new System.Drawing.Size(100, 22);
            this.toolStripProgressBar.Step = 1;
            this.toolStripProgressBar.Style = System.Windows.Forms.ProgressBarStyle.Continuous;
            this.toolStripProgressBar.Visible = false;
            // 
            // splitContainerRight
            // 
            this.splitContainerRight.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainerRight.Location = new System.Drawing.Point(0, 0);
            this.splitContainerRight.Name = "splitContainerRight";
            // 
            // splitContainerRight.Panel1
            // 
            this.splitContainerRight.Panel1.Controls.Add(this.structureTabControl);
            // 
            // splitContainerRight.Panel2
            // 
            this.splitContainerRight.Panel2.Controls.Add(this.infoTabControl);
            this.splitContainerRight.Size = new System.Drawing.Size(489, 583);
            this.splitContainerRight.SplitterDistance = 271;
            this.splitContainerRight.TabIndex = 0;
            // 
            // infoTabControl
            // 
            this.infoTabControl.Controls.Add(this.infoTabPage);
            this.infoTabControl.Controls.Add(this.filesTabPage);
            this.infoTabControl.Dock = System.Windows.Forms.DockStyle.Fill;
            this.infoTabControl.Location = new System.Drawing.Point(0, 0);
            this.infoTabControl.Name = "infoTabControl";
            this.infoTabControl.SelectedIndex = 0;
            this.infoTabControl.Size = new System.Drawing.Size(214, 583);
            this.infoTabControl.TabIndex = 0;
            // 
            // infoTabPage
            // 
            this.infoTabPage.Controls.Add(this.loaderImageInfoTab);
            this.infoTabPage.Location = new System.Drawing.Point(4, 22);
            this.infoTabPage.Name = "infoTabPage";
            this.infoTabPage.Padding = new System.Windows.Forms.Padding(3);
            this.infoTabPage.Size = new System.Drawing.Size(206, 557);
            this.infoTabPage.TabIndex = 0;
            this.infoTabPage.Text = "Info";
            this.infoTabPage.UseVisualStyleBackColor = true;
            // 
            // loaderImageInfoTab
            // 
            this.loaderImageInfoTab.Dock = System.Windows.Forms.DockStyle.Fill;
            this.loaderImageInfoTab.Image = ((System.Drawing.Image)(resources.GetObject("loaderImageInfoTab.Image")));
            this.loaderImageInfoTab.Location = new System.Drawing.Point(3, 3);
            this.loaderImageInfoTab.Name = "loaderImageInfoTab";
            this.loaderImageInfoTab.Size = new System.Drawing.Size(200, 551);
            this.loaderImageInfoTab.SizeMode = System.Windows.Forms.PictureBoxSizeMode.CenterImage;
            this.loaderImageInfoTab.TabIndex = 4;
            this.loaderImageInfoTab.TabStop = false;
            // 
            // filesTabPage
            // 
            this.filesTabPage.Controls.Add(this.filesTabToolStrip);
            this.filesTabPage.Controls.Add(this.filesTreeView);
            this.filesTabPage.Location = new System.Drawing.Point(4, 22);
            this.filesTabPage.Name = "filesTabPage";
            this.filesTabPage.Padding = new System.Windows.Forms.Padding(3);
            this.filesTabPage.Size = new System.Drawing.Size(206, 557);
            this.filesTabPage.TabIndex = 1;
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
            this.filesTabToolStrip.Size = new System.Drawing.Size(200, 25);
            this.filesTabToolStrip.TabIndex = 1;
            this.filesTabToolStrip.Text = "toolStrip1";
            // 
            // filesTabSearchBox
            // 
            this.filesTabSearchBox.Name = "filesTabSearchBox";
            this.filesTabSearchBox.Size = new System.Drawing.Size(100, 25);
            this.filesTabSearchBox.Text = "Search Files...";
            this.filesTabSearchBox.Enter += new System.EventHandler(this.searchBox_Enter);
            this.filesTabSearchBox.Leave += new System.EventHandler(this.searchBox_Leave);
            this.filesTabSearchBox.TextChanged += new System.EventHandler(this.filesTabSearchBox_TextChanged);
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
            this.showFileTypes_cob.Click += new System.EventHandler(this.settings_sourceCodeFiles_Click);
            // 
            // showFileTypes_txt
            // 
            this.showFileTypes_txt.CheckOnClick = true;
            this.showFileTypes_txt.Name = "showFileTypes_txt";
            this.showFileTypes_txt.Size = new System.Drawing.Size(160, 22);
            this.showFileTypes_txt.Text = "*.txt";
            this.showFileTypes_txt.Click += new System.EventHandler(this.settings_sourceCodeFiles_Click);
            // 
            // showFileTypes_src
            // 
            this.showFileTypes_src.CheckOnClick = true;
            this.showFileTypes_src.Name = "showFileTypes_src";
            this.showFileTypes_src.Size = new System.Drawing.Size(160, 22);
            this.showFileTypes_src.Text = "*.src";
            this.showFileTypes_src.Click += new System.EventHandler(this.settings_sourceCodeFiles_Click);
            // 
            // showFileTypes_custom
            // 
            this.showFileTypes_custom.Name = "showFileTypes_custom";
            this.showFileTypes_custom.Size = new System.Drawing.Size(100, 23);
            this.showFileTypes_custom.TextChanged += new System.EventHandler(this.settings_sourceCodeFiles_Click);
            // 
            // filesTreeView
            // 
            this.filesTreeView.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.filesTreeView.Location = new System.Drawing.Point(0, 31);
            this.filesTreeView.Name = "filesTreeView";
            this.filesTreeView.Size = new System.Drawing.Size(206, 523);
            this.filesTreeView.TabIndex = 0;
            this.filesTreeView.DoubleClick += new System.EventHandler(this.filesTreeView_DoubleClick);
            this.filesTreeView.KeyUp += new System.Windows.Forms.KeyEventHandler(this.filesTreeView_KeyUp);
            // 
            // FileControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.splitContainerTop);
            this.Margin = new System.Windows.Forms.Padding(2);
            this.Name = "FileControl";
            this.Size = new System.Drawing.Size(1173, 583);
            this.structureTabControl.ResumeLayout(false);
            this.tocTabPage.ResumeLayout(false);
            this.tocTabPage.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.loaderImageToc)).EndInit();
            this.tocToolStrip.ResumeLayout(false);
            this.tocToolStrip.PerformLayout();
            this.performsTabPage.ResumeLayout(false);
            this.performsTabPage.PerformLayout();
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
            this.infoTabControl.ResumeLayout(false);
            this.infoTabPage.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.loaderImageInfoTab)).EndInit();
            this.filesTabPage.ResumeLayout(false);
            this.filesTabPage.PerformLayout();
            this.filesTabToolStrip.ResumeLayout(false);
            this.filesTabToolStrip.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion
        private System.Windows.Forms.TreeView treeView;
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
        private System.Windows.Forms.TabPage filesTabPage;
        private System.Windows.Forms.TabPage infoTabPage;
        private System.Windows.Forms.TabControl infoTabControl;
        private System.Windows.Forms.ToolStrip codeViewToolStrip;
        private System.Windows.Forms.ToolStripTextBox searchBox;
        private System.Windows.Forms.ToolStripButton searchWithRegEx;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
        private System.Windows.Forms.ToolStripButton ResolveCopysButton;
        private System.Windows.Forms.ToolStrip filesTabToolStrip;
        private System.Windows.Forms.ToolStripTextBox filesTabSearchBox;
        private System.Windows.Forms.TreeView filesTreeView;
        private System.Windows.Forms.ToolStripDropDownButton fileTypeDropDownButton;
        private System.Windows.Forms.ToolStripMenuItem showFileTypes_cob;
        private System.Windows.Forms.ToolStripMenuItem showFileTypes_txt;
        private System.Windows.Forms.ToolStripMenuItem showFileTypes_src;
        private System.Windows.Forms.ToolStripTextBox showFileTypes_custom;
        private PictureBox loaderImageToc;
        private PictureBox loaderImageInfoTab;
        private ToolStripProgressBar toolStripProgressBar;
    }
}