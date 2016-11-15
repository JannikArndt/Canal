using Model.References;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.Serialization;

namespace Model.File
{
    /// <summary>
    /// Represents a real world COBOL file, usually is created by CobolFileBuilder-class
    /// For a structured description see https://github.com/JannikArndt/Canal/wiki/Model-classes
    /// </summary>
    [DataContract(IsReference = true)]
    public class CobolFile
    {
        /// <summary>
        /// Filename without extension
        /// </summary>
        [DataMember]
        public string Name { get; set; }

        /// <summary>
        /// Original file content
        /// </summary>
        [IgnoreDataMember]
        public string Text { get; set; }

        /// <summary>
        /// Info about the corresponding file on drive
        /// </summary>
        [DataMember]
        public FileReference FileReference { get; set; }

        /// <summary>
        /// A semantic view of the file contents
        /// </summary>
        [DataMember]
        public CobolTree CobolTree { get; set; }

        /// <summary>
        /// A list of all files that are references via COPY-Statement
        /// </summary>
        [DataMember]
        public List<FileReference> CopyReferences { get; set; }

        /// <summary>
        /// Contains the character index of all divisions and sections
        /// </summary>
        [DataMember]
        public DivisionAndSectionFlags DivisionsAndSection { get; set; }

        /// <summary>
        /// Dictionary of all variables
        /// </summary>
        [IgnoreDataMember]
        public ConcurrentDictionary<string, Variable> Variables { get; set; }

        /// <summary>
        /// Creates a new, (almost) empty CobolFile
        /// </summary>
        /// <param name="text">content of the file</param>
        /// <param name="name">filename without extension</param>
        public CobolFile(string text, string name)
        {
            Name = name;
            Text = text;
            CopyReferences = new List<FileReference>();
            Variables = new ConcurrentDictionary<string, Variable>();
        }

        /// <summary>
        /// Returns all LOCAL variables of level 1
        /// </summary>
        /// <returns></returns>
        public IEnumerable<Variable> GetLocalRootVariables()
        {
            return Variables.Values.Where(vari => vari.VariableLevel == 1 && vari.CopyReference.CobolFile == this);
        }

        /// <summary>
        /// Returns als COPIED variables of level 1
        /// </summary>
        /// <returns></returns>
        public IEnumerable<Variable> GetCopiedRootVariables()
        {
            return Variables.Values.Where(vari => vari.VariableLevel == 1 && vari.CopyReference.CobolFile != this);
        }
    }
}