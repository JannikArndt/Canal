using Canal.Properties;
using Model;
using Model.References;
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

        private static Dictionary<string, List<FileReference>> _directoriesWithAllowedFiles = new Dictionary<string, List<FileReference>>();

        private static List<string> _allowedEndings = new List<string>();

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

                FileReference reference;

                lock (Files)
                {
                    if (!Files.Any()) return null;
                    reference = Files[filename];
                }

                if (string.IsNullOrWhiteSpace(reference.FilePath))
                    reference.FilePath = filename;

                return Get(reference);
            }
            catch (Exception exception)
            {
                ErrorHandling.Exception(exception);
                throw new FileNotFoundException("File could not be loaded.", filename, exception);
            }
        }

        public static CobolFile Get(FileReference reference)
        {
            if (reference.CobolFile == null)
            {
                var lines = File.ReadAllText(reference.FilePath);

                reference.CobolFile = new CobolFile(lines, Path.GetFileNameWithoutExtension(reference.FilePath))
                {
                    FileReference = reference
                };
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

        public static List<FileReference> GetFileReferences(string programName)
        {
            if (string.IsNullOrWhiteSpace(programName))
                return null;

            Console.Write(@"Searching for Program " + programName);

            List<FileReference> candidates;

            lock (Files)
            {
                candidates = Files.Where(file => file.Key.Contains(programName)).Select(file => file.Value).ToList();
            }

            Console.WriteLine(candidates.Any() ? " => succeeded" : " => failed");

            return candidates;
        }

        public static FileReference GetFileReference(string programName, string folderName)
        {
            if (string.IsNullOrWhiteSpace(programName) || string.IsNullOrWhiteSpace(folderName))
                return null;

            lock (Files)
            {
                return Files.Where(file => file.Key.Contains(programName) && file.Key.Contains(folderName)).Select(file => file.Value).FirstOrDefault();
            }
        }

        /// <summary>
        /// Creates a cache of all subfolders and files in the current directory.
        /// </summary>
        /// <param name="fileOrFolderPath">A path with or without filename.</param>
        private static void AnalyzeFolder(string fileOrFolderPath)
        {
            if (RecentFolders.Contains(fileOrFolderPath) || !File.Exists(fileOrFolderPath))
                return;

            try
            {
                RecentFolders.Add(fileOrFolderPath);

                var folderPath = Path.GetDirectoryName(Path.GetDirectoryName(fileOrFolderPath));

                if (folderPath == null)
                    return;

                lock (Files)
                {
                    foreach (var fileSystemEntry in Directory.EnumerateFiles(folderPath, "*.*", SearchOption.AllDirectories)
                            .Where(file => !new FileInfo(file).Attributes.HasFlag(FileAttributes.Hidden | FileAttributes.System) && file.IndexOf(@"\.", StringComparison.Ordinal) < 0))
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
            }
            catch (Exception exception)
            {
                ErrorHandling.Exception(exception);
                throw new FileNotFoundException("Error analyzing file/folder.", fileOrFolderPath, exception);
            }
        }

        public static void ReduceDirectoriesToAllowedFiles()
        {
            _directoriesWithAllowedFiles = new Dictionary<string, List<FileReference>>();

            _allowedEndings = new List<string>();
            if (Settings.Default.FileTypeCob) _allowedEndings.AddRange(new List<string> { ".cob", ".cbl" });
            if (Settings.Default.FileTypeTxt) _allowedEndings.Add(".txt");
            if (Settings.Default.FileTypeCob) _allowedEndings.Add(".src");
            if (!string.IsNullOrWhiteSpace(Settings.Default.FileTypeCustom)) _allowedEndings.Add(Settings.Default.FileTypeCustom);

            foreach (var dir in DirectoriesAndFiles.Keys)
            {
                var tempDir = new List<FileReference>(DirectoriesAndFiles[dir].Where(file => file.FilePath.HasAllowedEnding()));
                if (tempDir.Any())
                    _directoriesWithAllowedFiles.Add(dir, tempDir);
            }
        }

        public static TreeNode[] GetDirectoryStructure(string query = "")
        {
            if (_directoriesWithAllowedFiles == null || !_directoriesWithAllowedFiles.Any())
                ReduceDirectoriesToAllowedFiles();

            var result = new List<TreeNode>();

            // ReSharper disable once PossibleNullReferenceException Nope
            foreach (var dir in _directoriesWithAllowedFiles.Keys.OrderBy(key => key))
            {
                var foundFiles = _directoriesWithAllowedFiles[dir]
                    .Where(file => CultureInfo.CurrentCulture.CompareInfo.IndexOf(file.ProgramName, query, CompareOptions.IgnoreCase) >= 0)
                    .Select(file => new TreeNode(file.ProgramName) { Tag = file }).ToArray();

                if (foundFiles.Any())
                    result.Add(new TreeNode(dir, foundFiles));
            }

            return result.ToArray();
        }

        private static bool HasAllowedEnding(this string text)
        {
            return _allowedEndings.Contains(text.Substring(text.Length - 4, 4).ToLowerInvariant());
        }
    }
}
