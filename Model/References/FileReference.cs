using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace Model.References
{
    public class FileReference
    {
        public FileReference(string fileSystemEntry)
        {
            ReferencedIn = new List<Procedure>();
            FullPath = fileSystemEntry;
            ProgramName = Path.GetFileNameWithoutExtension(fileSystemEntry);
            var folder = Path.GetDirectoryName(fileSystemEntry);
            if (folder != null)
                Directory = folder.TrimEnd(Path.DirectorySeparatorChar).Substring(folder.LastIndexOf(Path.DirectorySeparatorChar) + 1);
        }

        public string ProgramName { get; set; }

        public string Directory { get; set; }

        public string FullPath { get; set; }

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