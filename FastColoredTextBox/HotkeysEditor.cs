using System;
using System.ComponentModel;
using System.Drawing.Design;
using System.Windows.Forms;
using System.Windows.Forms.Design;

namespace FastColoredTextBoxNS
{
    internal class HotkeysEditor : UITypeEditor
    {
        public override UITypeEditorEditStyle GetEditStyle(System.ComponentModel.ITypeDescriptorContext context)
        {
            return UITypeEditorEditStyle.Modal;
        }

        public override object EditValue(ITypeDescriptorContext context, IServiceProvider provider, object value)
        {
            if ((provider != null) && (((IWindowsFormsEditorService)provider.GetService(typeof(IWindowsFormsEditorService))) != null))
            {
                var form = new HotkeysEditorForm(Hotkeys.Parse(value as string));

                if (form.ShowDialog() == DialogResult.OK)
                    value = form.GetHotkeys().ToString();
            }
            return value;
        }
    }
}