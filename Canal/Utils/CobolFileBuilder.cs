using Logging;
using Model;
using Model.References;
using System.IO;

namespace Canal.Utils
{
    public class CobolFileBuilder
    {
        /// <summary>
        /// Loads the text from the file given in the filename parameter and creates a new CobolFile object.
        /// If filename is empty, a blank CobolFile is created.
        /// </summary>
        public CobolFile Build(string filename)
        {
            if (string.IsNullOrWhiteSpace(filename))
            {
                Logger.Info("Creating a new CobolFile.");
                return new CobolFile("", "New File");
            }

            Logger.Info("Finding reference for filename {0}.", filename);

            var fileReference = FileUtil.Instance.GetFileReference(filename);

            return Build(fileReference);
        }

        /// <summary>
        /// Loads the text from the file given in the reference and creates a new CobolFile object.
        /// </summary>
        public CobolFile Build(FileReference fileReference)
        {
            Logger.Info("Loading text for reference {0}.", fileReference);
            var text = FileUtil.Instance.GetText(fileReference);

            var name = Path.GetFileNameWithoutExtension(fileReference.FilePath);

            // Create new CobolFile
            var file = new CobolFile(text, name)
            {
                FileReference = fileReference,
            };

            // Set relation: reference <=> file
            fileReference.CobolFile = file;

            Logger.Info("Built new CobolFile {0}.", file.Name);

            return file;
        }
    }
}
