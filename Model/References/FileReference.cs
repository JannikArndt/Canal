using Model.File;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.Serialization;

namespace Model.References
{
    [DataContract(IsReference = true)]
    public class FileReference
    {
        public FileReference(string fileSystemEntry)
        {
            ReferencedIn = new List<Procedure>();
            FilePath = fileSystemEntry;
            ProgramName = Path.GetFileNameWithoutExtension(fileSystemEntry);
            var folder = Path.GetDirectoryName(fileSystemEntry);
            if (folder != null)
                Directory = folder.TrimEnd(Path.DirectorySeparatorChar).Substring(folder.LastIndexOf(Path.DirectorySeparatorChar) + 1);
        }

        /// <summary>
        /// Filename without extension
        /// </summary>
        [DataMember]
        public string ProgramName { get; private set; }

        /// <summary>
        /// Name of leaf-directory
        /// </summary>
        [DataMember]
        public string Directory { get; private set; }

        /// <summary>
        /// Full path
        /// </summary>
        [DataMember]
        public string FilePath { get; private set; }

        [DataMember]
        public CobolFile CobolFile { get; set; }

        [DataMember]
        public List<Procedure> ReferencedIn { get; private set; }

        public override string ToString()
        {
            if (ReferencedIn.Any())
                return string.Format("{0} > {1} ({2})", Directory, ProgramName, string.Join(", ", ReferencedIn));

            return string.Format("{0} > {1}", Directory, ProgramName);
        }
    }
}