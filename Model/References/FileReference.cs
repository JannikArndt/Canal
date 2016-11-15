using Model.File;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.Serialization;

namespace Model.References
{
    /// <summary>
    /// Contains all information about a physical file on drive
    /// </summary>
    [DataContract(IsReference = true)]
    public class FileReference
    {
        /// <summary>
        /// Creates an info-object from the given path (<paramref name="fileSystemEntry"/>)
        /// </summary>
        /// <param name="fileSystemEntry"></param>
        public FileReference(string fileSystemEntry)
        {
            ReferencedIn = new List<Procedure>();
            FilePath = fileSystemEntry;
            FileExtension = Path.GetExtension(fileSystemEntry);
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
        /// The extension
        /// </summary>
        [DataMember]
        public string FileExtension { get; private set; }

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

        /// <summary>
        /// Reference to canal-representation as a <see cref="CobolFile"/>
        /// </summary>
        [DataMember]
        public CobolFile CobolFile { get; set; }

        /// <summary>
        /// List of <see cref="Procedure"/> that call this file
        /// </summary>
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