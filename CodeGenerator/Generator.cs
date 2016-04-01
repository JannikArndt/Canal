using System.IO;
using System.Linq;
using System.Text;

namespace CodeGenerator
{
    public class Generator
    {
        public string GenerateBusinessObject(GeneratorConfiguration config)
        {
            var text = new StringBuilder(File.ReadAllText("Resources/BusinessObjectExample.cs"));
            text.Replace("<!NAMESPACE!>", config.Namespace);
            text.Replace("<!BUSINESSOBJECTNAME!>", config.BusinessObjectName);
            text.Replace("<!PROPERTIES!>", string.Join("\n        \n", config.Variables.Select(GenerateProperty)));

            return text.ToString();
        }

        private string GenerateProperty(GeneratorModel variable)
        {
            return string.Format("        /// <summary>\n        /// {0}\n        /// [{1}]\n        /// </summary>\n        public {2} {3} {{ get; set; }}",
                    variable.Comment, variable.CobolVariableName, variable.GeneratedCodeType, variable.PropertyName);
        }

        public string GenerateMapper(GeneratorConfiguration config)
        {
            var text = new StringBuilder(File.ReadAllText("Resources/MapperExample.cs"));
            text.Replace("<!NAMESPACE!>", config.Namespace);
            text.Replace("<!BUSINESSOBJECTNAME!>", config.BusinessObjectName);
            text.Replace("<!BONAMEPARAMETER!>", config.BusinessObjectNameAsParameter);
            text.Replace("<!BYTEARRAYSIZE!>", config.ByteArraySize.ToString());
            text.Replace("<!MAPFROMCOBOL!>", string.Join("\n        \n", config.Variables.Select(GenerateMappingFromCobol)));
            text.Replace("<!MAPTOCOBOL!>", string.Join("\n        \n", config.Variables.Select(vari => GenerateMappingToCobol(vari, config.BusinessObjectName))));

            return text.ToString();
        }

        private string GenerateMappingFromCobol(GeneratorModel variable)
        {
            var mapping = "GetString(0, 10)";

            return string.Format("                 {0} = bytes.{1}", variable.PropertyName, mapping);
        }

        private string GenerateMappingToCobol(GeneratorModel variable, string className)
        {
            return string.Format("            bytes.SetString(0, 10, {0}.{1})", className, variable.PropertyName);
        }
    }
}
