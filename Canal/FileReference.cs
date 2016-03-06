using System.IO;

namespace Canal
{
    internal class FileReference
    {
        public FileReference(string fileSystemEntry)
        {
            FullPath = fileSystemEntry;
            ProgramName = Path.GetFileNameWithoutExtension(fileSystemEntry);
            Directory = Path.GetDirectoryName(fileSystemEntry);
        }

        public string ProgramName { get; set; }

        public string Directory { get; set; }

        public string FullPath { get; set; }

        public CobolFile CobolFile { get; set; }
    }
}