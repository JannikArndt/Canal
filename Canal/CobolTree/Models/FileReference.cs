
namespace Canal.CobolTree.Models
{
    public class FileReference
    {
        public string FileName { get; set; }

        public string FolderName { get; set; }

        public CobolFile File { get; set; }
    }
}
