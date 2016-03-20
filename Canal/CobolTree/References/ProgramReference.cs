
namespace Canal.CobolTree
{
    public abstract class ProgramReference
    {
        public string FileName { get; set; }

        public string FolderName { get; set; }

        public CobolFile File { get; set; }
    }
}
