using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace Canal
{
    public static class FileUtil
    {
        private static List<string> _recentFolders = new List<string>();

        private static Dictionary<string, FileReference> _files = new Dictionary<string, FileReference>();

        /// <summary>
        /// Loads the given file. Uses internal cache for file contents.
        /// </summary>
        /// <param name="filename">An exact filepath.</param>
        /// <returns>A CobolFile with the file contents as Text.</returns>
        public static CobolFile Get(string filename)
        {
            if (string.IsNullOrWhiteSpace(filename))
                return null;

            AnalyzeFolder(filename);

            var reference = _files[filename];

            if (reference.CobolFile == null)
            {
                var lines = File.ReadAllText(filename);

                reference.CobolFile = new CobolFile(lines, Path.GetFileNameWithoutExtension(filename));
            }

            return reference.CobolFile;
        }

        /// <summary>
        /// Searches all know directories for a file- and folder-match and returns the first match.
        /// If the folder is not found, but the program name has exactly one match, that program is returned.
        /// Uses an internal cache for directory-contents and previously loaded file contents.
        /// </summary>
        /// <param name="programName">The name of a program, without extension.</param>
        /// <param name="folderName">The name of a directory, no full path required.</param>
        /// <param name="includeParentDirectory">If the file is not found, the parent directory and all its subdirectories are analyzed as well.</param>
        /// <returns>A CobolFile with the file contents as Text.</returns>
        public static CobolFile Get(string programName, string folderName, bool includeParentDirectory = true)
        {
            if (string.IsNullOrWhiteSpace(programName))
                return null;

            Console.Write("Searching for Program " + programName + " in folder " + folderName);

            var candidate = _files.Keys.FirstOrDefault(key => key.Contains(programName) && key.Contains(folderName));

            if (candidate == null && _files.Keys.Count(key => key.Contains(programName)) == 1)
                candidate = _files.Keys.FirstOrDefault(key => key.Contains(programName));

            if (candidate == null)
            {
                foreach (var knownFolder in _recentFolders)
                {
                    AnalyzeFolder(Path.GetDirectoryName(knownFolder));
                    return Get(programName, folderName, false);
                }
            }

            Console.WriteLine(candidate != null ? " => succeeded" : " => failed");

            return Get(candidate);
        }

        /// <summary>
        /// Creates a cache of all subfolders and files in the current directory.
        /// </summary>
        /// <param name="fileOrFolderPath">A path with or without filename.</param>
        private static void AnalyzeFolder(string fileOrFolderPath)
        {
            if (_recentFolders.Contains(fileOrFolderPath))
                return;

            _recentFolders.Add(fileOrFolderPath);

            var folderPath = Path.GetDirectoryName(fileOrFolderPath);

            foreach (var fileSystemEntry in Directory.EnumerateFiles(folderPath, "*.*", SearchOption.AllDirectories)
                .Where(s => s.EndsWith(".cob", StringComparison.OrdinalIgnoreCase)
                         || s.EndsWith(".cbl", StringComparison.OrdinalIgnoreCase)
                         || s.EndsWith(".txt", StringComparison.OrdinalIgnoreCase)))
            {
                if (!_files.ContainsKey(fileSystemEntry))
                {
                    _files.Add(fileSystemEntry, new FileReference(fileSystemEntry));
                }
            }

        }
    }


}
