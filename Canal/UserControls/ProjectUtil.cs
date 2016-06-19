using System.ComponentModel;
using Model.Project;
using Util;

namespace Canal.UserControls
{
    public class ProjectUtil
    {
        public static readonly ProjectUtil Instance = new ProjectUtil();

        private ProjectUtil()
        {
        }

        public void CreateNewProject()
        {
            var assistant = new ProjectAssistant();
            assistant.ShowDialog();

        }

        public void CreateProject(object sender, DoWorkEventArgs e)
        {
            var project = e.Argument as CobolProject;
            if(project == null)
                return;

            FileUtil.Instance.AnalyzeFolder(project.FilePath);

        }
    }
}
