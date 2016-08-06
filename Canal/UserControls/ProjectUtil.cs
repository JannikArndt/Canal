using Canal.Properties;
using Logging;
using Model.File;
using Model.Project;
using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.Serialization;
using System.Threading.Tasks;
using System.Windows.Forms;
using Util;

namespace Canal.UserControls
{
    public class ProjectUtil
    {
        public static readonly ProjectUtil Instance = new ProjectUtil();

        public CobolProject Current { get; set; }

        public string CurrentFilename { get; set; }

        private ProjectUtil()
        {
        }

        public void ShowProjectAssistant()
        {
            try
            {
                var assistant = new ProjectAssistant();
                assistant.ShowDialog();

                if (!assistant.Success || assistant.Cancelled)
                    return;

                Current = assistant.Result;
            }
            catch (Exception exception)
            {
                Logger.Error(exception, "Error on ProjectAssistant: {0}", exception.Message);
            }

        }

        public void LoadFiles(object sender, DoWorkEventArgs e)
        {
            var project = e.Argument as CobolProject;
            if (project == null)
                return;

            var worker = (BackgroundWorker)sender;
            worker.WorkerReportsProgress = true;

            FileUtil.Instance.AnalyzeFolder(project.FilesRoot);

            var references = FileUtil.Instance.GetFileReferences(project.FilesRoot);

            Parallel.ForEach(references, fileReference =>
            {
                var text = File.ReadAllText(fileReference.FilePath);

                var name = Path.GetFileNameWithoutExtension(fileReference.FilePath);

                // Create new CobolFile
                var cobolFile = new CobolFile(text, name)
                {
                    FileReference = fileReference,
                };

                // Set relation: reference <=> file
                fileReference.CobolFile = cobolFile;

                project.Files.TryAdd(fileReference, cobolFile);

                var progress = (int)((float)project.Files.Count / references.Count * 100);
                worker.ReportProgress(progress);
            });

            e.Result = project;
        }

        public void Save(string fileName)
        {
            try
            {
                using (var fileStream = new FileStream(fileName, FileMode.Create, FileAccess.ReadWrite))
                {
                    var ser = new DataContractSerializer(typeof(CobolProject));
                    ser.WriteObject(fileStream, Current);
                }

            }
            catch (Exception exception)
            {
                Logger.Error(exception, "Error trying to save project at path {0}: {1}", fileName, exception.Message);
                MessageBox.Show("Error saving project: " + exception.Message, Resources.Error, MessageBoxButtons.OK);
            }

        }

        public void Open(string fileName)
        {
            try
            {
                using (var fileStream = new FileStream(fileName, FileMode.Open, FileAccess.Read))
                {
                    var ser = new DataContractSerializer(typeof(CobolProject));
                    ser.ReadObject(fileStream);
                }
            }
            catch (Exception exception)
            {
                Logger.Error(exception, "Error trying to open project at path {0}: {1}", fileName, exception.Message);
                MessageBox.Show("Error opening project: " + exception.Message, Resources.Error, MessageBoxButtons.OK);
            }
        }
    }
}