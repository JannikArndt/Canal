using FastColoredTextBoxNS.Enums;
using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;

namespace CodeGenerator
{
    public partial class CodeGeneratorMainWindow : Form
    {
        private readonly GeneratorConfiguration _configuration = new GeneratorConfiguration();

        public CodeGeneratorMainWindow()
        {
            InitializeComponent();

            FakeConfig();

            // Configuration Tab

            BindingSource bindingSource = new BindingSource(_configuration.Variables, null);

            ConfigurationDataGridView.DataSource = bindingSource;

            var generator = new Generator();

            // Business Object Tab
            BusinessObjectCodeBox.Language = Language.CSharp;
            BusinessObjectCodeBox.Text = generator.GenerateBusinessObject(_configuration);

            // Mapper Tab
            MapperCodeBox.Language = Language.CSharp;
            MapperCodeBox.Text = generator.GenerateMapper(_configuration);

            // Extensions Tab
            ExtensionsCodeBox.Language = Language.CSharp;
            ExtensionsCodeBox.Text = File.ReadAllText("Resources/ByteArrayExtensions.cs");

            // Enums Tab
            EnumsCodeBox.Language = Language.CSharp;
            EnumsCodeBox.Text = File.ReadAllText("Resources/EnumExample.cs");

        }

        private void FakeConfig()
        {
            _configuration.CobolFileName = "MyCobolFile";
            _configuration.BusinessObjectName = "MyBusinessObject";
            _configuration.Namespace = "Projects.MyNamespace";
            _configuration.Variables = new List<GeneratorModel>
            {
                new GeneratorModel(true, "MY-TEXT", CobolVariableTypes.PicX, "MyText", GeneratedCodeTypes.String, string.Empty, "Some text"),
                new GeneratorModel(true, "MY-NUMBER", CobolVariableTypes.PicS9, "MyNumber", GeneratedCodeTypes.Int, string.Empty, "Some number")
                };
        }
    }
}
