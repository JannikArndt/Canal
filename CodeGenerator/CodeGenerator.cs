using FastColoredTextBoxNS.Enums;
using System.ComponentModel;
using System.IO;
using System.Windows.Forms;

namespace CodeGenerator
{
    public partial class CodeGeneratorMainWindow : Form
    {
        public BindingList<GeneratorModel> Variables { get; set; }

        public CodeGeneratorMainWindow()
        {
            InitializeComponent();

            // Configuration Tab

            Variables = new BindingList<GeneratorModel>();

            Variables.Add(new GeneratorModel());

            BindingSource bindingSource = new BindingSource(Variables, null);

            ConfigurationDataGridView.DataSource = bindingSource;

            // Business Object Tab
            BusinessObjectCodeBox.Language = Language.CSharp;
            BusinessObjectCodeBox.Text = File.ReadAllText("Resources/BusinessObjectExample.cs");

            // Mapper Tab
            MapperCodeBox.Language = Language.CSharp;
            MapperCodeBox.Text = File.ReadAllText("Resources/MapperExample.cs");

            // Extensions Tab
            ExtensionsCodeBox.Language = Language.CSharp;
            ExtensionsCodeBox.Text = File.ReadAllText("Resources/ByteArrayExtensions.cs");

            // Enums Tab
            EnumsCodeBox.Language = Language.CSharp;
            EnumsCodeBox.Text = File.ReadAllText("Resources/EnumExample.cs");

        }
    }
}
