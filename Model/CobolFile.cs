using Model.References;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;

namespace Model
{
    public class CobolFile
    {
        public string Name { get; set; }

        public string Text { get; set; }

        public FileReference FileReference { get; set; }

        public CobolTree CobolTree { get; set; }

        public List<FileReference> CopyReferences { get; set; }

        public DivisionAndSectionFlags DivisionsAndSection { get; set; }

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