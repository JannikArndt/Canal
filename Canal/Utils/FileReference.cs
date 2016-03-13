namespace Canal.Utils
{
    using System.IO;

    internal class FileReference
    {
        public FileReference(string fileSystemEntry)
        {
            FullPath = fileSystemEntry;
            ProgramName = Path.GetFileNameWithoutExtension(fileSystemEntry);
            var folder = Path.GetDirectoryName(fileSystemEntry);
            Directory = folder.TrimEnd(Path.DirectorySeparatorChar).Substring(folder.LastIndexOf(Path.DirectorySeparatorChar) + 1);
        }

        public string ProgramName { get; set; }

        public string Directory { get; set; }

        public string FullPath { get; set; }

        public CobolFile CobolFile { get; set; }

        public override string ToString()
        {
            return string.Format("{0} > {1}", Directory, ProgramName);
        }
    }
}