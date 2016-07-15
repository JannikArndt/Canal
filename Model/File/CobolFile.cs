using Model.References;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.Serialization;

namespace Model.File
{
    [DataContract(IsReference = true)]
    public class CobolFile
    {
        [DataMember]
        public string Name { get; set; }

        [IgnoreDataMember]
        public string Text { get; set; }

        [DataMember]
        public FileReference FileReference { get; set; }

        [DataMember]
        public CobolTree CobolTree { get; set; }

        [DataMember]
        public List<FileReference> CopyReferences { get; set; }

        [DataMember]
        public DivisionAndSectionFlags DivisionsAndSection { get; set; }

        [IgnoreDataMember]
        public ConcurrentDictionary<string, Variable> Variables { get; set; }

        public CobolFile(string text, string name)
        {
            Name = name;
            Text = text;
            CopyReferences = new List<FileReference>();
            Variables = new ConcurrentDictionary<string, Variable>();
        }

        public IEnumerable<Variable> GetLocalRootVariables()
        {
            return Variables.Values.Where(vari => vari.VariableLevel == 1 && vari.CopyReference.CobolFile == this);
        }

        public IEnumerable<Variable> GetCopiedRootVariables()
        {
            return Variables.Values.Where(vari => vari.VariableLevel == 1 && vari.CopyReference.CobolFile != this);
        }
    }
}