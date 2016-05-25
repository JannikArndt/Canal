using System;
using System.Xml.Serialization;

namespace CodeGenerator
{
    public class SerializeableGeneratorModel
    {
        [XmlAttribute]
        public bool DoMap { get; set; }

        [XmlAttribute]
        public int Level { get; set; }

        [XmlAttribute]
        public string CobolName { get; set; }

        [XmlAttribute]
        public string Picture { get; set; }

        [XmlAttribute]
        public int Offset { get; set; }

        [XmlAttribute]
        public int Bytes { get; set; }

        [XmlAttribute]
        public string PropertyName { get; set; }

        [XmlAttribute]
        public string Type { get; set; }

        [XmlAttribute]
        public string Mapper { get; set; }

        [XmlAttribute]
        public string Comment { get; set; }

        public SerializeableGeneratorModel()
        {

        }

        public SerializeableGeneratorModel(GeneratorModel generatorModel)
        {
            DoMap = generatorModel.DoMap;
            Level = generatorModel.Variable.VariableLevel;
            CobolName = generatorModel.CobolVariableName;
            Picture = generatorModel.VariableType;
            Offset = generatorModel.Offset;
            Bytes = generatorModel.Variable.ByteLength;
            PropertyName = generatorModel.PropertyName;
            Type = generatorModel.GeneratedCodeType.ToString();
            Mapper = generatorModel.MapperName;
            Comment = generatorModel.Comment;
        }

        public GeneratorModel ToGeneratorModel()
        {
            return new GeneratorModel(null, PropertyName,
                (GeneratedCodeTypes)Enum.Parse(typeof(GeneratedCodeTypes), Type), Comment)
            {
                PropertyName = PropertyName,
                DoMap = DoMap,
                MapperName = Mapper
            };
        }
    }
}
