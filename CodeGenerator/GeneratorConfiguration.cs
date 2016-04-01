using System.Collections.Generic;

namespace CodeGenerator
{
    public class GeneratorConfiguration
    {
        public List<GeneratorModel> Variables { get; set; }

        public string CobolFileName { get; set; }

        public string BusinessObjectName { get; set; }

        public string BusinessObjectNameAsParameter
        {
            get { return char.ToLowerInvariant(BusinessObjectName[0]) + BusinessObjectName.Substring(1); }
        }

        public string Namespace { get; set; }

        public int ByteArraySize { get; set; }
    }
}
