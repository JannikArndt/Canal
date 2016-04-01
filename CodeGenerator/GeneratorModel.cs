using System.ComponentModel;

namespace CodeGenerator
{
    public class GeneratorModel
    {
        [DisplayName("Map?")]
        public bool DoMap { get; set; }

        [DisplayName("Cobol Variable Name")]
        public string CobolVariableName { get; set; }

        [DisplayName("Variable Type")]
        public CobolVariableTypes VariableType { get; set; }

        [DisplayName("Generated Property Name")]
        public string PropertyName { get; set; }

        [DisplayName("Generated Property Type")]
        public GeneratedCodeTypes GeneratedCodeType { get; set; }

        [DisplayName("Use Mapper")]
        public string MapperName { get; set; }

        [DisplayName("Comment")]
        public string Comment { get; set; }
    }
}
