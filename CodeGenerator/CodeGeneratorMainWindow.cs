using CodeGenerator.Properties;
using FastColoredTextBoxNS.Enums;
using Model;
using Model.Pictures;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Windows.Forms;
using System.Xml.Serialization;
using Util;

namespace CodeGenerator
{
    public partial class CodeGeneratorMainWindow : Form
    {
        private GeneratorConfiguration _configuration = new GeneratorConfiguration();

        public CodeGeneratorMainWindow(CobolFile cobolFile)
        {
            InitializeComponent();

            InitDataGridView();

            BuildRootVariableTree(cobolFile);

            // Business Object Tab
            BusinessObjectCodeBox.Language = Language.CSharp;

            // Mapper Tab
            MapperCodeBox.Language = Language.CSharp;

            // Extensions Tab
            ExtensionsCodeBox.Language = Language.CSharp;

            // Enums Tab
            EnumsCodeBox.Language = Language.CSharp;
        }

        private void BuildRootVariableTree(CobolFile cobolFile)
        {
            AddRootVariable("Local", cobolFile.GetLocalRootVariables());

            foreach (var group in cobolFile.GetCopiedRootVariables().GroupBy(vari => vari.CopyReference.ProgramName))
                AddRootVariable(group.Key, group);

            VariableTreeView.ExpandAll();
        }

        private void AddRootVariable(string name, IEnumerable<Variable> variables)
        {
            var nodes = variables.Select(variable => new TreeNode(variable.VariableName) { Tag = variable }).ToArray();

            VariableTreeView.Nodes.Add(new TreeNode(name, nodes));
        }

        private void ShowVariableConfiguration(Variable rootVar)
        {
            CodeGeneratorTabControl.SelectedIndex = 1;

            _configuration.CobolFileName = rootVar.CopyReference.ProgramName;
            _configuration.BusinessObjectName = rootVar.VariableName.ToPropertyName();
            _configuration.Namespace = "Projects.MyNamespace";
            var vars = BuildFlatVariableList(rootVar);
            _configuration.Variables = vars.Cast<GeneratorModel>().ToList();

            // Configuration Tab
            var bindingList = new BindingList<IMappingProvider>(vars);
            BindingSource bindingSource = new BindingSource(bindingList, null);
            ConfigurationDataGridView.DataSource = bindingSource;
        }

        private void InitDataGridView()
        {
            ConfigurationDataGridView.Columns.Add(new DataGridViewCheckBoxColumn
            {
                Name = "Map?",
                DataPropertyName = "DoMap"
            });

            ConfigurationDataGridView.Columns.Add(new DataGridViewTextBoxColumn
            {
                Name = "Level",
                DataPropertyName = "Level",
                ReadOnly = true,
                Width = 80
            });

            ConfigurationDataGridView.Columns.Add(new DataGridViewTextBoxColumn
            {
                Name = "Name",
                DataPropertyName = "CobolVariableName",
                ReadOnly = true,
                Width = 100
            });

            ConfigurationDataGridView.Columns.Add(new DataGridViewTextBoxColumn
            {
                Name = "Picture",
                DataPropertyName = "VariableType",
                ReadOnly = true,
                Width = 100
            });

            ConfigurationDataGridView.Columns.Add(new DataGridViewTextBoxColumn
            {
                Name = "Offset",
                DataPropertyName = "Offset",
                ReadOnly = true,
                Width = 30
            });

            ConfigurationDataGridView.Columns.Add(new DataGridViewTextBoxColumn
            {
                Name = "Bytes",
                DataPropertyName = "Bytes",
                ReadOnly = true,
                Width = 30
            });

            ConfigurationDataGridView.Columns.Add(new DataGridViewComboBoxColumn
            {
                Name = "Type",
                DataPropertyName = "GeneratedCodeType",
                DataSource = Enum.GetValues(typeof(GeneratedCodeTypes)),
                ValueType = typeof(GeneratedCodeTypes),
                Width = 50
            });

            ConfigurationDataGridView.Columns.Add(new DataGridViewTextBoxColumn
            {
                Name = "Property",
                DataPropertyName = "PropertyName",
                Width = 150
            });

            ConfigurationDataGridView.Columns.Add(new DataGridViewComboBoxColumn
            {
                Name = "Mapper",
                DataPropertyName = "MapperName",
                DataSource = new[] { "None", "Mapper1", "Mapper2" },
                ValueType = typeof(string),
                Width = 150
            });

            ConfigurationDataGridView.Columns.Add(new DataGridViewTextBoxColumn
            {
                Name = "Comment",
                DataPropertyName = "Comment",
                Width = 200
            });


            ConfigurationDataGridView.AutoGenerateColumns = false;
        }

        private List<IMappingProvider> BuildFlatVariableList(Variable rootVar)
        {
            var result = new List<IMappingProvider>
            {
                new GeneratorModel(rootVar, rootVar.VariableName.ToPropertyName(), MapType(rootVar.Picture), rootVar.VariableDefinition)
            };

            foreach (var variable in rootVar.Variables)
            {
                result.AddRange(BuildFlatVariableList(variable));
            }

            return result;
        }

        private GeneratedCodeTypes MapType(IPic pic)
        {
            if (pic is PicX)
                if (pic.Length > 1)
                    return GeneratedCodeTypes.@string;
                else
                    return GeneratedCodeTypes.@bool;

            if (pic is Pic88)
                return GeneratedCodeTypes.@bool;

            if (pic is PicS9V9 || pic is Pic9V9)
                return GeneratedCodeTypes.@decimal;

            if (pic is PicS9 || pic is Pic9)
                if (pic.Length < 10)
                    return GeneratedCodeTypes.@int;
                else
                    return GeneratedCodeTypes.@long;

            return GeneratedCodeTypes.@object;
        }

        private void VariableTreeView_AfterSelect(object sender, TreeViewEventArgs e)
        {
            if (e.Node.Tag is Variable)
                ShowVariableConfiguration(e.Node.Tag as Variable);
        }

        private void BusinessObjectTabPage_Paint(object sender, PaintEventArgs e)
        {
            if (_configuration.Variables == null)
                return;

            BusinessObjectCodeBox.Text = Generator.Instance.GenerateBusinessObject(_configuration);
        }

        private void MapperTabPage_Paint(object sender, PaintEventArgs e)
        {
            if (_configuration.Variables == null)
                return;

            MapperCodeBox.Text = Generator.Instance.GenerateMapper(_configuration);
        }

        private void ExtensionMethods_Paint(object sender, PaintEventArgs e)
        {
            if (string.IsNullOrEmpty(ExtensionsCodeBox.Text))
                ExtensionsCodeBox.Text = Resources.ByteArrayExtensions;
        }

        private void EnumsTabPage_Paint(object sender, PaintEventArgs e)
        {
            if (string.IsNullOrEmpty(EnumsCodeBox.Text))
                EnumsCodeBox.Text = Resources.EnumExample;
        }

        private void SaveConfigurationClick(object sender, EventArgs e)
        {
            saveFileDialog1.Filter = @"Configuration XML|*.xml";

            if (saveFileDialog1.ShowDialog() == DialogResult.OK)
            {
                var serializer = new XmlSerializer(typeof(GeneratorConfiguration));
                using (TextWriter writer = new StreamWriter(saveFileDialog1.FileName))
                {
                    serializer.Serialize(writer, _configuration);
                }
            }
        }

        private void toolStripButton3_Click(object sender, EventArgs e)
        {
            var openFileDialog = new OpenFileDialog
            {
                Filter = @"Configuration XML|*.xml"
            };
            if (openFileDialog.ShowDialog() != DialogResult.OK)
                return;


            using (var fileStream = new FileStream(openFileDialog.FileName, FileMode.Open))
            {
                var serializer = new XmlSerializer(typeof(GeneratorConfiguration));
                var config = (GeneratorConfiguration)serializer.Deserialize(fileStream);

                _configuration = config;
            }

            if (_configuration.Variables == null)
                return;

            // Configuration Tab
            var bindingList = new BindingList<IMappingProvider>(_configuration.Variables.Cast<IMappingProvider>().ToList());
            BindingSource bindingSource = new BindingSource(bindingList, null);
            ConfigurationDataGridView.DataSource = bindingSource;
        }
    }
}
