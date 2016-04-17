using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Windows.Forms;

namespace Model.References
{
    public class FileReference : TreeNode
    {
        public FileReference(string fileSystemEntry) : base(Path.GetFileNameWithoutExtension(fileSystemEntry))
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
        public string ProgramName { get; set; }

        /// <summary>
        /// Name of leaf-directory
        /// </summary>
        public string Directory { get; set; }

        /// <summary>
        /// Full path
        /// </summary>
        public string FilePath { get; set; }

        public CobolFile CobolFile { get; set; }

        public List<Procedure> ReferencedIn { get; set; }

        public override string ToString()
        {
            if (ReferencedIn.Any())
                return string.Format("{0} > {1} ({2})", Directory, ProgramName, string.Join(", ", ReferencedIn));

            return string.Format("{0} > {1}", Directory, ProgramName);
        }
    }
}