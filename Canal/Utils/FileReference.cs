namespace Canal.Utils
{
    using System.IO;

    internal class FileReference
    {
        public FileReference(string fileSystemEntry)
        {
            this.FullPath = fileSystemEntry;
            this.ProgramName = Path.GetFileNameWithoutExtension(fileSystemEntry);
            this.Directory = Path.GetDirectoryName(fileSystemEntry);
        }

        public string ProgramName { get; set; }

        public string Directory { get; set; }

        public string FullPath { get; set; }

        public CobolFile CobolFile { get; set; }
    }
}