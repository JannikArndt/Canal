using FastColoredTextBoxNS.Enums;
using Model;
using Model.Pictures;
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
            _configuration.Variables = new List<IMappingProvider>
            {
                new GeneratorModel(new Variable(1, "MY-TEXT", new PicX(9), string.Empty, null), "MyText", GeneratedCodeTypes.String, "My String"),
                new GeneratorModel(new Variable(1, "MY-NUMBER", new PicS9(2),  string.Empty, null), "MyNumber", GeneratedCodeTypes.Int, "My Number")
                };
        }
    }
}
