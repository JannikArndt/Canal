using Canal.Utils;
using Model.References;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;

namespace Canal.UserControls
{
    public partial class FileReferenceFilter : Form
    {
        public IEnumerable<FileReference> Result { get; private set; }

        public FileReferenceFilter(List<FileReference> references)
        {
            InitializeComponent();

            referenceListBox.Font = SourceCodePro.Regular;

            // ReSharper disable once StringCompareToIsCultureSpecific
            references.Sort((ref1, ref2) => ref1.ToString().CompareTo(ref2.ToString()));
            referenceListBox.DataSource = references;

            button_selectAll_Click(this, null);
        }

        protected override bool ProcessCmdKey(ref Message msg, Keys keyData)
        {
            if (keyData == Keys.Escape)
                Close();

            if (keyData == Keys.Enter)
                button_ok_Click(this, null);

            return base.ProcessCmdKey(ref msg, keyData);
        }

        private void button_ok_Click(object sender, System.EventArgs e)
        {
            Result = referenceListBox.CheckedItems.Cast<FileReference>().ToList();
            Close();
        }

        private void button_cancel_Click(object sender, System.EventArgs e)
        {
            Close();
        }

        private void button_selectAll_Click(object sender, System.EventArgs e)
        {
            for (int i = 0; i < referenceListBox.Items.Count; i++)
            {
                referenceListBox.SetItemChecked(i, true);
            }
        }

        private void button_selectNone_Click(object sender, System.EventArgs e)
        {
            for (int i = 0; i < referenceListBox.Items.Count; i++)
            {
                referenceListBox.SetItemChecked(i, false);
            }
        }
    }
}
