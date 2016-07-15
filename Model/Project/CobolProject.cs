using Model.File;
using Model.References;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Runtime.Serialization;

namespace Model.Project
{
    [DataContract]
    public class CobolProject
    {
        private readonly ConcurrentDictionary<FileReference, CobolFile> _files;

        [DataMember]
        public string Name { get; set; }

        [DataMember]
        public string FilesRoot { get; private set; }

        [DataMember]
        public List<string> FileTypes { get; set; }

        [DataMember]
        public ConcurrentDictionary<FileReference, CobolFile> Files
        {
            get { return _files ?? new ConcurrentDictionary<FileReference, CobolFile>(); }
        }

        public CobolProject(string name, string filesRoot, List<string> fileTypes)
        {
            Name = name;
            FilesRoot = filesRoot;
            FileTypes = fileTypes;
            _files = new ConcurrentDictionary<FileReference, CobolFile>();
        }
    }
}
