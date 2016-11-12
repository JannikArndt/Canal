using CodeGenerator.Properties;
using System.Linq;
using System.Text;

namespace CodeGenerator
{
    public class Generator
    {
        public static readonly Generator Instance = new Generator();

        private Generator()
        {
        }

        public string GenerateBusinessObject(GeneratorConfiguration config)
        {
            var text = new StringBuilder(Resources.BusinessObjectExample);
            text.Replace("<!NAMESPACE!>", config.Namespace);
            text.Replace("<!BUSINESSOBJECTNAME!>", config.BusinessObjectName);
            text.Replace("<!PROPERTIES!>", string.Join("\n        \n", config.Variables.Where(vari => vari.DoMap).Select(vari => vari.GetPropertyDefinition())));

            return text.ToString();
        }

        public string GenerateMapper(GeneratorConfiguration config)
        {
            var text = new StringBuilder(Resources.MapperExample);
            var variables = config.Variables.Where(vari => vari.DoMap).ToList();
            text.Replace("<!NAMESPACE!>", config.Namespace);
            text.Replace("<!BUSINESSOBJECTNAME!>", config.BusinessObjectName);
            text.Replace("<!BONAMEPARAMETER!>", config.BusinessObjectNameAsParameter);
            text.Replace("<!BYTEARRAYSIZE!>", config.Variables.Find(vari => vari.Variable.VariableLevel == 1).Bytes);
            text.Replace("<!MAPFROMCOBOL!>", string.Join("\n        \n", variables.Select(vari => vari.GetMappingFromCobol())));
            text.Replace("<!MAPTOCOBOL!>", string.Join("\n        \n", variables.Select(vari => vari.GetMappingToCobol(config.BusinessObjectNameAsParameter))));

            return text.ToString();
        }
    }
}
