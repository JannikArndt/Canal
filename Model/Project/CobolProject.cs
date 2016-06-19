using System.Collections.Generic;

namespace Model.Project
{
    public class CobolProject
    {
        public string Name { get; set; }

        public string FilePath { get; set; }

        public List<string> FileTypes { get; set; }

        public CobolProject(string name, string filePath, List<string> fileTypes)
        {
            Name = name;
            FilePath = filePath;
            FileTypes = fileTypes;
        }
    }
}
