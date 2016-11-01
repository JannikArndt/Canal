using Logging;
using Model.References;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Windows.Forms;
using Model.Exceptions;
using Util.Events;
using Util.Properties;

namespace Util
{
    public class FileUtil
    {
        private static FileUtil _instance;

        public static FileUtil Instance
        {
            get { return _instance ?? (_instance = new FileUtil()); }
        }

        public event EventHandler<FileCacheChangedEventArgs> FileCacheChanged;

        private readonly List<string> _recentFolders = new List<string>();

        private readonly ConcurrentDictionary<string, FileReference> _files = new ConcurrentDictionary<string, FileReference>();

        private readonly ConcurrentDictionary<string, List<FileReference>> _directoriesAndFiles = new ConcurrentDictionary<string, List<FileReference>>();

        private ConcurrentDictionary<string, List<FileReference>> _directoriesWithAllowedFiles = new ConcurrentDictionary<string, List<FileReference>>();

        private List<string> _allowedEndings = new List<string>();

        /// <summary>
        /// Loads the given file. Uses internal cache for file contents.
        /// </summary>
        /// <param name="filename">An exact filepath.</param>
        /// <returns>A CobolFile with the file contents as Text.</returns>
        public FileReference GetFileReference(string filename)
        {
            if (string.IsNullOrWhiteSpace(filename))
            {
                Logger.Warning("Trying to load file with empty filename.");
                return null;
            }
            try
            {
                if (_files.ContainsKey(filename))
                {
                    Logger.Info("Loading file from cache: {0}.", filename);
                    return _files[filename];
                }

                Logger.Info("File {0} is not in cache. Loading from disk.", filename);

                var newRef = new FileReference(filename);
                _files.TryAdd(filename, newRef);
                return newRef;
            }
            catch (Exception exception)
            {
                Logger.Error("Error loading file {0}: {1}.", filename, exception.Message);
                throw new FileNotFoundException("File could not be loaded.", filename, exception);
            }
        }

        public string GetText(FileReference reference)
        {
            if (string.IsNullOrWhiteSpace(reference.FilePath))
                if (string.IsNullOrWhiteSpace(reference.Directory))
                    if (string.IsNullOrWhiteSpace(reference.ProgramName))
                        throw new ArgumentException(@"FileReference is empty.", "reference");
                    else
                        reference = GetFileReferences(reference.ProgramName).FirstOrDefault();
                else
                    reference = GetFileReference(reference.ProgramName, reference.Directory);

            if (reference == null)
                return null;

            return File.ReadAllText(reference.FilePath);
        }

        public List<FileReference> GetFileReferences(string programName)
        {
            if (string.IsNullOrWhiteSpace(programName))
                return null;

            Console.Write(@"Searching for Program " + programName);

            var candidates = _files.AsParallel().Where(file => file.Key.Contains(programName)).Select(file => file.Value).ToList();

            Console.WriteLine(candidates.Any() ? " => succeeded" : " => failed");

            return candidates;
        }

        public FileReference GetFileReference(string programName, string folderName)
        {
            if (string.IsNullOrWhiteSpace(programName) || string.IsNullOrWhiteSpace(folderName))
                return null;

            return _files.AsParallel().Where(file => file.Key.Contains(programName + ".") && file.Key.Contains(folderName)).Select(file => file.Value).First();
        }

        /// <summary>
        /// Method to find file references (more specific in the using context of this method: file references to copybooks) without a given parent folder.
        /// Because there is no distinct location given, the selection of the file reference is based on a searching pattern:
        /// If there is only one file with the given name in the cache, it's selected. If there are more than one, it's checked if only one of them ends
        /// with the extension *.cbl. If that is true, this file reference is selected, if not an exception is thrown. This method could also search directly
        /// for the filename + extension but that would cut out the possibility of loading files with functionally not correct extension like *.txt (in case their
        /// name is unique).
        /// </summary>
        /// <param name="programName">The name of the program the file reference is wanted for.</param>
        /// <returns>The file reference for the program.</returns>
        public FileReference GetFileReferenceWithoutKnownFolderName(string programName)
        {
            if (string.IsNullOrWhiteSpace(programName))
                return null;


            //Selecting all files with the programName in the file cache ignoring the file extension.
            var allFileReferencesWithGivenName =  _files.AsParallel().Where(file => file.Key.Contains(programName + ".")).Select(file => file.Value).ToList();


            if (!allFileReferencesWithGivenName.Any())
                //No file found, also no chance of finding it using the more precise filters below
                throw new CopiedRessourceNotFoundException(programName);
            else if (allFileReferencesWithGivenName.Count() > 1)
            {
                //If more than one file of that name is found, a more specific search is done, including the file extension.
                allFileReferencesWithGivenName =
                    _files.AsParallel()
                        .Where(file => file.Key.Contains(programName + ".cbl"))
                        .Select(file => file.Value)
                        .ToList();


                if (allFileReferencesWithGivenName.Count() != 1)
                    //If there is still more than one file (or no file now), an exception is thrown, stating the fact, that a distinct file selection is impossible.
                    throw new CopiedRessourceNotIdentifiedDistinctlyByNameException(programName);
            } 

            //Else the found reference is returned.
            return allFileReferencesWithGivenName.First();
            
        }

        /// <summary>
        /// Creates a cache of all subfolders and files in the current directory.
        /// </summary>
        public void AnalyzeFolder(string fileOrFolderPath)
        {
            if (_recentFolders.Contains(fileOrFolderPath))
                return;

            if (!File.Exists(fileOrFolderPath))
            {
                Logger.Error("Trying to analyze non-existent file or folder {0}.", fileOrFolderPath);
                return;
            }

            try
            {
                var folderPath = Path.GetDirectoryName(Path.GetDirectoryName(fileOrFolderPath));

                if (folderPath == null)
                    return;

                foreach (var newRef in GetRelevantFileNames(folderPath).AsParallel())
                {
                    _files.TryAdd(newRef.FilePath, newRef);
                    if (!_directoriesAndFiles.ContainsKey(newRef.Directory))
                        _directoriesAndFiles.TryAdd(newRef.Directory, new List<FileReference>());

                    _directoriesAndFiles[newRef.Directory].Add(newRef);
                }

                _recentFolders.Add(fileOrFolderPath);
            }
            catch (Exception exception)
            {
                Logger.Error("Error analyzing file/folder {0}: {1}", fileOrFolderPath, exception.Message);
                throw new FileNotFoundException("Error analyzing file/folder.", fileOrFolderPath, exception);
            }
            finally
            {
                if (FileCacheChanged != null) FileCacheChanged(this, new FileCacheChangedEventArgs());
            }
        }

        private IEnumerable<FileReference> GetRelevantFileNames(string path)
        {
            return GetDirectoryFiles(path, "*.*").AsParallel()
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
                foreach (string dir in subDirs.AsParallel())
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

        private void UpdateAllowedEndings()
        {
            _allowedEndings = new List<string>();
            if (Settings.Default.FileTypeCob)
                _allowedEndings.Add(".cob");
            if (Settings.Default.FileTypeCbl)
                _allowedEndings.Add(".cbl");
            if (Settings.Default.FileTypeTxt)
                _allowedEndings.Add(".txt");
            if (Settings.Default.FileTypeCob)
                _allowedEndings.Add(".src");
            if (!string.IsNullOrWhiteSpace(Settings.Default.FileTypeCustom)) _allowedEndings.Add(Settings.Default.FileTypeCustom);
        }

        public void ReduceDirectoriesToAllowedFiles()
        {
            _directoriesWithAllowedFiles = new ConcurrentDictionary<string, List<FileReference>>();

            UpdateAllowedEndings();

            foreach (var dir in _directoriesAndFiles.Keys.AsParallel())
            {
                var tempDir = new List<FileReference>(_directoriesAndFiles[dir].Where(HasAllowedEnding));
                if (tempDir.Any())
                    _directoriesWithAllowedFiles.TryAdd(dir, tempDir);
            }
        }

        public int CountValidFiles(string dir)
        {
            UpdateAllowedEndings();

            AnalyzeFolder(dir);

            var filenames = new HashSet<string>();
            foreach (var refs in _directoriesAndFiles.Values)
                foreach (var filename in refs.Select(fileRef => fileRef.FilePath).Where(HasAllowedEnding).Where(path => path.Contains(dir)))
                    filenames.Add(filename);

            return filenames.Count;
        }

        public TreeNode[] GetDirectoryStructure(string query = "")
        {
            if (_directoriesWithAllowedFiles == null || !_directoriesWithAllowedFiles.Any())
                ReduceDirectoriesToAllowedFiles();

            var result = new List<TreeNode>();

            // ReSharper disable once PossibleNullReferenceException Nope
            foreach (var dir in _directoriesWithAllowedFiles.Keys.OrderBy(key => key).AsParallel())
            {
                var foundFiles = _directoriesWithAllowedFiles[dir]
                    .Where(file => CultureInfo.CurrentCulture.CompareInfo.IndexOf(file.ProgramName, query, CompareOptions.IgnoreCase) >= 0)
                    .Select(file => new TreeNode(file.ProgramName) { Tag = file }).ToArray();

                if (foundFiles.Any())
                    result.Add(new TreeNode(dir, foundFiles));
            }

            return result.ToArray();
        }

        private bool HasAllowedEnding(FileReference fileReference)
        {
            if (fileReference == null)
                return false;

            return HasAllowedEnding(fileReference.FilePath);
        }

        private bool HasAllowedEnding(string text)
        {
            return _allowedEndings.Contains(text.Substring(text.Length - 4, 4).ToLowerInvariant());
        }
    }
}
