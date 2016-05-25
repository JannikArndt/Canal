using System.Collections.Generic;
using System.Linq;
using System.Xml.Serialization;

namespace CodeGenerator
{
    public class GeneratorConfiguration
    {
        [XmlIgnore]
        public List<GeneratorModel> Variables { get; set; }

        public List<SerializeableGeneratorModel> SerializableVariables
        {
            get
            {
                return Variables == null
                    ? new List<SerializeableGeneratorModel>()
                    : Variables.Select(generatorModel => new SerializeableGeneratorModel(generatorModel)).ToList();
            }

            set
            {
                Variables = new List<GeneratorModel>();

                foreach (var serializeableGeneratorModel in value)
                {
                    Variables.Add(serializeableGeneratorModel.ToGeneratorModel());
                }
            }
        }

        public string CobolFileName { get; set; }

        public string BusinessObjectName { get; set; }

        [XmlIgnore]
        public string BusinessObjectNameAsParameter
        {
            get { return char.ToLowerInvariant(BusinessObjectName[0]) + BusinessObjectName.Substring(1); }
        }

        public string Namespace { get; set; }
    }
}
