using System.Globalization;
using System.Windows.Forms;

namespace Canal.Utils
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Linq;

    public static class FileUtil
    {
        private static readonly List<string> RecentFolders = new List<string>();

        private static readonly Dictionary<string, FileReference> Files = new Dictionary<string, FileReference>();

        private static readonly Dictionary<string, List<FileReference>> DirectoriesAndFiles = new Dictionary<string, List<FileReference>>();

        /// <summary>
        /// Loads the given file. Uses internal cache for file contents.
        /// </summary>
        /// <param name="filename">An exact filepath.</param>
        /// <returns>A CobolFile with the file contents as Text.</returns>
        public static CobolFile Get(string filename)
        {
            if (string.IsNullOrWhiteSpace(filename))
                return null;

            try
            {
                AnalyzeFolder(filename);

                var reference = Files[filename];

                if (reference.CobolFile == null)
                {
                    var lines = File.ReadAllText(filename);

                    reference.CobolFile = new CobolFile(lines, Path.GetFileNameWithoutExtension(filename))
                    {
                        FileReference = reference
                    };
                }

                return reference.CobolFile;
            }
            catch (Exception exception)
            {
                ErrorHandling.Exception(exception);
                throw new FileNotFoundException("File could not be loaded.", filename, exception);
            }
        }

        /// <summary>
        /// Searches all know directories for a file- and folder-match and returns the first match.
        /// If the folder is not found, but the program name has exactly one match, that program is returned.
        /// Uses an internal cache for directory-contents and previously loaded file contents.
        /// </summary>
        /// <param name="programName">The name of a program, without extension.</param>
        /// <param name="folderName">The name of a directory, no full path required.</param>
        /// <returns>A CobolFile with the file contents as Text.</returns>
        public static CobolFile Get(string programName, string folderName)
        {
            if (string.IsNullOrWhiteSpace(programName))
                return null;

            Console.Write(@"Searching for Program " + programName + @" in folder " + folderName);

            var candidate = Files.Keys.FirstOrDefault(key => key.Contains(programName) && key.Contains(folderName));

            if (candidate == null && Files.Keys.Count(key => key.Contains(programName)) == 1)
                candidate = Files.Keys.FirstOrDefault(key => key.Contains(programName));

            if (candidate == null)
            {
                foreach (var knownFolder in RecentFolders)
                {
                    AnalyzeFolder(Path.GetDirectoryName(knownFolder));
                    return Get(programName, folderName);
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
            if (RecentFolders.Contains(fileOrFolderPath))
                return;

            try
            {
                RecentFolders.Add(fileOrFolderPath);

                var folderPath = Path.GetDirectoryName(Path.GetDirectoryName(fileOrFolderPath));

                if (folderPath == null)
                    return;

                foreach (var fileSystemEntry in Directory.EnumerateFiles(folderPath, "*.*", SearchOption.AllDirectories)
                    .Where(
                        s => s.EndsWith(".cob", StringComparison.OrdinalIgnoreCase) || s.EndsWith(".cbl", StringComparison.OrdinalIgnoreCase)
                            || s.EndsWith(".txt", StringComparison.OrdinalIgnoreCase) || s.EndsWith(".src", StringComparison.OrdinalIgnoreCase)))
                {
                    if (!Files.ContainsKey(fileSystemEntry))
                    {
                        var newRef = new FileReference(fileSystemEntry);
                        Files.Add(fileSystemEntry, newRef);
                        if (!DirectoriesAndFiles.ContainsKey(newRef.Directory))
                            DirectoriesAndFiles.Add(newRef.Directory, new List<FileReference>());

                        DirectoriesAndFiles[newRef.Directory].Add(newRef);
                    }
                }
            }
            catch (Exception exception)
            {
                ErrorHandling.Exception(exception);
                throw new FileNotFoundException("Error analyzing file/folder.", fileOrFolderPath, exception);
            }
        }

        public static TreeNode[] GetDirectoryStructure(string query = "")
        {
            var result = new List<TreeNode>();

            foreach (var dir in DirectoriesAndFiles.Keys.OrderBy(key => key))
            {
                if (string.IsNullOrWhiteSpace(query))
                    result.Add(new TreeNode(dir,
                        DirectoriesAndFiles[dir].Select(file => new TreeNode(file.ProgramName) { Tag = file }).ToArray()));
                else
                {
                    var foundFiles = DirectoriesAndFiles[dir]
                        .Where(file => CultureInfo.CurrentCulture.CompareInfo.IndexOf(file.ProgramName, query, CompareOptions.IgnoreCase) >= 0)
                        .Select(file => new TreeNode(file.ProgramName) { Tag = file }).ToArray();
                    if (foundFiles.Any())
                        result.Add(new TreeNode(dir, foundFiles));
                }
            }

            return result.ToArray();
        }
    }


}
