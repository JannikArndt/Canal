using Canal.Utils;
using System;
using System.Linq;
using System.Windows.Forms;

namespace Canal.UserControls
{
    public partial class FileInfo : UserControl
    {
        private readonly CobolFile _file;

        private readonly MainWindow _parent;

        public SplitterPanel VariableInfoPanel;

        public FileInfo(CobolFile cobolFile, MainWindow parent)
        {
            InitializeComponent();
            VariableInfoPanel = splitContainer.Panel2;

            _file = cobolFile;
            _parent = parent;

            headingLabel.Text = _file.Name;

            infoDataGridView.DataSource = _file.Infos.ToArray();

            callReferencesListBox.DataSource = _file.CallReferences;

        }

        private void callReferencesListBox_DoubleClick(object sender, EventArgs e)
        {
            var fileRef = callReferencesListBox.SelectedItem as FileReference;
            if (fileRef != null)
                _parent.OpenFile(fileRef.FullPath);
        }
    }
}
