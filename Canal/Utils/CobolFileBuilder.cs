using Model;
using Model.References;
using System.IO;
using System.Linq;

namespace Canal.Utils
{
    public class CobolFileBuilder
    {
        public static readonly CobolFileBuilder Instance = new CobolFileBuilder();

        private CobolFileBuilder()
        {
        }

        public CobolFile Build(string filename)
        {
            if (string.IsNullOrWhiteSpace(filename))
            {
                return new CobolFile("", "New File");
            }

            var fileReference = FileUtil.Instance.GetFileReference(filename);

            return Build(fileReference);
        }

        public CobolFile Build(FileReference fileReference)
        {
            var text = FileUtil.Instance.GetText(fileReference);

            var name = Path.GetFileNameWithoutExtension(fileReference.FilePath);

            var file = new CobolFile(text, name)
            {
                FileReference = fileReference,
                CopyReferences = ReferenceUtil.Instance.FindCopyReferences(text).ToList()
            };

            return file;
        }
    }
}
