using Canal.Events;
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

    public class FileUtil
    {
        private static FileUtil _instance;

        public static FileUtil Instance
        {
            get { return _instance ?? (_instance = new FileUtil()); }
        }

        public event EventHandler<FileCacheChangedEventArgs> FileCacheChanged;

        private readonly List<string> _recentFolders = new List<string>();

        private readonly Dictionary<string, FileReference> _files = new Dictionary<string, FileReference>();

        private readonly Dictionary<string, List<FileReference>> _directoriesAndFiles = new Dictionary<string, List<FileReference>>();

        private Dictionary<string, List<FileReference>> _directoriesWithAllowedFiles = new Dictionary<string, List<FileReference>>();

        private List<string> _allowedEndings = new List<string>();

        private readonly BackgroundWorker _fileCacheWorker = new BackgroundWorker();

        /// <summary>
        /// Loads the given file. Uses internal cache for file contents.
        /// </summary>
        /// <param name="filename">An exact filepath.</param>
        /// <returns>A CobolFile with the file contents as Text.</returns>
        public CobolFile Get(string filename)
        {
            if (string.IsNullOrWhiteSpace(filename))
            {
                Logger.Warning("Trying to load file with empty filename.");
                return null;
            }
            try
            {
                lock (_files)
                {
                    if (_files.ContainsKey(filename))
                    {
                        Logger.Info("Loading file from cache: {0}.", filename);
                        return Get(_files[filename]);
                    }

                    Logger.Info("File {0} is not in cache. Loading from disk and triggering background analysis.", filename);

                    _fileCacheWorker.DoWork += AnalyzeFolder;
                    _fileCacheWorker.RunWorkerCompleted += (sender, args) =>
                    {
                        Logger.Info("Completed background filesystem analysis. Cache built.");
                        if (FileCacheChanged != null) FileCacheChanged(sender, new FileCacheChangedEventArgs());
                    };
                    _fileCacheWorker.RunWorkerAsync(filename);

                    var newRef = new FileReference(filename);
                    _files.Add(filename, newRef);
                    return Get(newRef);
                }
            }
            catch (Exception exception)
            {
                ErrorHandling.Exception(exception);
                throw new FileNotFoundException("File could not be loaded.", filename, exception);
            }
        }

        public CobolFile Get(FileReference reference)
        {
            if (reference.CobolFile != null) return reference.CobolFile;

            var lines = File.ReadAllText(reference.FilePath);

            reference.CobolFile = new CobolFile(lines, Path.GetFileNameWithoutExtension(reference.FilePath))
            {
                FileReference = reference
            };

            return reference.CobolFile;
        }

        public List<FileReference> GetFileReferences(string programName)
        {
            if (string.IsNullOrWhiteSpace(programName))
                return null;

            Console.Write(@"Searching for Program " + programName);

            List<FileReference> candidates;

            lock (_files)
            {
                candidates = _files.Where(file => file.Key.Contains(programName)).Select(file => file.Value).ToList();
            }

            Console.WriteLine(candidates.Any() ? " => succeeded" : " => failed");

            return candidates;
        }

        public FileReference GetFileReference(string programName, string folderName)
        {
            if (string.IsNullOrWhiteSpace(programName) || string.IsNullOrWhiteSpace(folderName))
                return null;

            lock (_files)
            {
                return _files.Where(file => file.Key.Contains(programName) && file.Key.Contains(folderName)).Select(file => file.Value).FirstOrDefault();
            }
        }

        /// <summary>
        /// Creates a cache of all subfolders and files in the current directory.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="doWorkEventArgs">An event containing the path as argument</param>
        private void AnalyzeFolder(object sender, DoWorkEventArgs doWorkEventArgs)
        {
            var fileOrFolderPath = doWorkEventArgs.Argument.ToString();

            if (_recentFolders.Contains(fileOrFolderPath))
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

                lock (_files)
                {
                    foreach (var newRef in GetRelevantFileNames(folderPath))
                    {
                        _files.Add(newRef.FilePath, newRef);
                        if (!_directoriesAndFiles.ContainsKey(newRef.Directory))
                            _directoriesAndFiles.Add(newRef.Directory, new List<FileReference>());

                        _directoriesAndFiles[newRef.Directory].Add(newRef);
                    }
                }
                _recentFolders.Add(fileOrFolderPath);
            }
            catch (Exception exception)
            {
                Logger.Error("Error analyzing file/folder {0}: {1}", fileOrFolderPath, exception.Message);
                throw new FileNotFoundException("Error analyzing file/folder.", fileOrFolderPath, exception);
            }
        }

        private IEnumerable<FileReference> GetRelevantFileNames(string path)
        {
            return GetDirectoryFiles(path, "*.*")
                .Where(file =>
                        !new FileInfo(file).Attributes.HasFlag(FileAttributes.Hidden | FileAttributes.System) &&
                        file.IndexOf(@"\.", StringComparison.Ordinal) < 0 &&
                        !_files.ContainsKey(file))
                        .Select(file => new FileReference(file));
        }

        /// <summary>
        /// A safe way to get all the files in a directory and sub directory without crashing on UnauthorizedException or PathTooLongException
        /// from http://stackoverflow.com/a/20719754
        /// </summary>
        /// <param name="rootPath">Starting directory</param>
        /// <param name="patternMatch">Filename pattern match</param>
        /// <returns>List of files</returns>
        private IEnumerable<string> GetDirectoryFiles(string rootPath, string patternMatch)
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

        public void ReduceDirectoriesToAllowedFiles()
        {
            _directoriesWithAllowedFiles = new Dictionary<string, List<FileReference>>();

            _allowedEndings = new List<string>();
            if (Settings.Default.FileTypeCob) _allowedEndings.AddRange(new List<string> { ".cob", ".cbl" });
            if (Settings.Default.FileTypeTxt) _allowedEndings.Add(".txt");
            if (Settings.Default.FileTypeCob) _allowedEndings.Add(".src");
            if (!string.IsNullOrWhiteSpace(Settings.Default.FileTypeCustom)) _allowedEndings.Add(Settings.Default.FileTypeCustom);

            foreach (var dir in _directoriesAndFiles.Keys)
            {
                var tempDir = new List<FileReference>(_directoriesAndFiles[dir].Where(file => HasAllowedEnding(file.FilePath)));
                if (tempDir.Any())
                    _directoriesWithAllowedFiles.Add(dir, tempDir);
            }
        }

        public TreeNode[] GetDirectoryStructure(string query = "")
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

        private bool HasAllowedEnding(string text)
        {
            return _allowedEndings.Contains(text.Substring(text.Length - 4, 4).ToLowerInvariant());
        }
    }
}
