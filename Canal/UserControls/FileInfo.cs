using Model;
using Model.References;
using System;
using System.Linq;
using System.Windows.Forms;

namespace Canal.UserControls
{
    public partial class FileInfo : UserControl
    {
        private readonly MainWindow _parent;

        public SplitterPanel VariableInfoPanel;

        public FileInfo(CobolFile cobolFile, MainWindow parent)
        {
            InitializeComponent();
            VariableInfoPanel = splitContainer.Panel2;

            _parent = parent;

            headingLabel.Text = cobolFile.Name;

            infoDataGridView.DataSource = cobolFile.Infos.ToArray();

            callReferencesListBox.DataSource = cobolFile.CallReferences;

        }

        private void callReferencesListBox_DoubleClick(object sender, EventArgs e)
        {
            var fileRef = callReferencesListBox.SelectedItem as FileReference;
            if (fileRef != null)
                _parent.OpenFile(fileRef.FullPath);
        }
    }
}
