using Model;
using Model.References;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;

namespace Canal.UserControls.WordInfoViews
{
    public partial class ProgramInfo : UserControl
    {
        private readonly FileControl _parent;

        public ProgramInfo(CobolFile cobolFile, FileControl parent)
        {
            InitializeComponent();
            _parent = parent;

            FillCallTreeView(cobolFile);
        }

        private void FillCallTreeView(CobolFile cobolFile)
        {
            if (cobolFile.CobolTree == null)
            {
                return;
            }

            var callNode = new TreeNode("Calls");
            wordInfoTreeView.Nodes.Add(callNode);

            var uniqueFolders = new HashSet<string>(cobolFile.CobolTree.CallReferences.Select(cr => cr.Directory));

            foreach (var folder in uniqueFolders)
            {
                var folderNode = new TreeNode(folder);
                callNode.Nodes.Add(folderNode);
                var folder1 = folder;
                foreach (var fileRef in cobolFile.CobolTree.CallReferences.Where(cr => cr.Directory == folder1))
                {
                    folderNode.Nodes.Add(new TreeNode(fileRef.ProgramName) { Tag = fileRef });
                }
            }

            wordInfoTreeView.ExpandAll();

            wordInfoTreeView.NodeMouseDoubleClick += (sender, args) =>
            {
                var fileRef = wordInfoTreeView.SelectedNode.Tag as FileReference;
                if (fileRef != null)
                    _parent.MainWindow.OpenFile(fileRef.FilePath);
            };
        }

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            wordInfoTreeView.Nodes.Clear();
            wordInfoTreeView.Dispose();

            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }
    }
}
