using Canal.Properties;
using Logging;
using Model;
using Model.References;
using System.ComponentModel;
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
        public static event EventHandler<FileCacheChangedEventArgs> FileCacheChanged;

        private static readonly List<string> RecentFolders = new List<string>();

        private static readonly Dictionary<string, FileReference> Files = new Dictionary<string, FileReference>();

        private static readonly Dictionary<string, List<FileReference>> DirectoriesAndFiles = new Dictionary<string, List<FileReference>>();

        private static Dictionary<string, List<FileReference>> _directoriesWithAllowedFiles = new Dictionary<string, List<FileReference>>();

        private static List<string> _allowedEndings = new List<string>();

        private static readonly BackgroundWorker FileCacheWorker = new BackgroundWorker();

        /// <summary>
        /// Loads the given file. Uses internal cache for file contents.
        /// </summary>
        /// <param name="filename">An exact filepath.</param>
        /// <returns>A CobolFile with the file contents as Text.</returns>
        public static CobolFile Get(string filename)
        {
            if (string.IsNullOrWhiteSpace(filename))
            {
                Logger.Warning("Trying to load file with empty filename.");
                return null;
            }
            try
            {
                lock (Files)
                {
                    if (Files.ContainsKey(filename))
                    {
                        Logger.Info("Loading file from cache: {0}.", filename);
                        return Get(Files[filename]);
                    }

                    Logger.Info("File {0} is not in cache. Loading from disk and triggering background analysis.", filename);

                    FileCacheWorker.DoWork += AnalyzeFolder;
                    FileCacheWorker.RunWorkerCompleted += (sender, args) =>
                    {
                        Logger.Info("Completed background filesystem analysis. Cache built.");
                        if (FileCacheChanged != null) FileCacheChanged(sender, new FileCacheChangedEventArgs());
                    };
                    FileCacheWorker.RunWorkerAsync(filename);

                    var newRef = new FileReference(filename);
                    Files.Add(filename, newRef);
                    return Get(newRef);
                }
            }
            catch (Exception exception)
            {
                ErrorHandling.Exception(exception);
                throw new FileNotFoundException("File could not be loaded.", filename, exception);
            }
        }

        public static CobolFile Get(FileReference reference)
        {
            if (reference.CobolFile != null) return reference.CobolFile;

            var lines = File.ReadAllText(reference.FilePath);

            reference.CobolFile = new CobolFile(lines, Path.GetFileNameWithoutExtension(reference.FilePath))
            {
                FileReference = reference
            };

            return reference.CobolFile;
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
        /// <param name="sender"></param>
        /// <param name="doWorkEventArgs">An event containing the path as argument</param>
        private static void AnalyzeFolder(object sender, DoWorkEventArgs doWorkEventArgs)
        {
            var fileOrFolderPath = doWorkEventArgs.Argument.ToString();

            if (RecentFolders.Contains(fileOrFolderPath))
                return;

            if (!File.Exists(fileOrFolderPath))
            {
                Logger.Error("Trying to analyze non-existent file or folder {0}.", fileOrFolderPath);
            }

            try
            {
                var folderPath = Path.GetDirectoryName(Path.GetDirectoryName(fileOrFolderPath));

                if (folderPath == null)
                    return;

                lock (Files)
                {
                    foreach (var newRef in GetRelevantFileNames(folderPath))
                    {
                        Files.Add(newRef.FilePath, newRef);
                        if (!DirectoriesAndFiles.ContainsKey(newRef.Directory))
                            DirectoriesAndFiles.Add(newRef.Directory, new List<FileReference>());

                        DirectoriesAndFiles[newRef.Directory].Add(newRef);
                    }
                }
                RecentFolders.Add(fileOrFolderPath);
            }
            catch (Exception exception)
            {
                Logger.Error("Error analyzing file/folder {0}: {1}", fileOrFolderPath, exception.Message);
                throw new FileNotFoundException("Error analyzing file/folder.", fileOrFolderPath, exception);
            }
        }

        private static IEnumerable<FileReference> GetRelevantFileNames(string path)
        {
            return GetDirectoryFiles(path, "*.*")
                .Where(file =>
                        !new FileInfo(file).Attributes.HasFlag(FileAttributes.Hidden | FileAttributes.System) &&
                        file.IndexOf(@"\.", StringComparison.Ordinal) < 0 &&
                        !Files.ContainsKey(file))
                        .Select(file => new FileReference(file));
        }

        /// <summary>
        /// A safe way to get all the files in a directory and sub directory without crashing on UnauthorizedException or PathTooLongException
        /// from http://stackoverflow.com/a/20719754
        /// </summary>
        /// <param name="rootPath">Starting directory</param>
        /// <param name="patternMatch">Filename pattern match</param>
        /// <returns>List of files</returns>
        private static IEnumerable<string> GetDirectoryFiles(string rootPath, string patternMatch)
        {
            // ReSharper disable PossibleMultipleEnumeration

            var foundFiles = Enumerable.Empty<string>();

            try
            {
                IEnumerable<string> subDirs = Directory.EnumerateDirectories(rootPath);
                foreach (string dir in subDirs)
                {
                    foundFiles = foundFiles.Concat(GetDirectoryFiles(dir, patternMatch)); // Add files in subdirectories recursively to the list
                }
            }
            catch (UnauthorizedAccessException) { }
            catch (PathTooLongException) { }

            try
            {
                foundFiles = foundFiles.Concat(Directory.EnumerateFiles(rootPath, patternMatch)); // Add files from the current directory
            }
            catch (UnauthorizedAccessException) { }

            return foundFiles;
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
