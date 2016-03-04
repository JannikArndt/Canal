using System.Text;
using System.Windows.Forms;

namespace Canal.Windows
{
    using System.Collections.Generic;
    using System.Linq;

    using Canal.CobolTree.Models;

    public partial class Performs : Form
    {
        public Performs(CobolFile cobolFile)
        {
            this.InitializeComponent();

            var text = new StringBuilder();
            var sections = cobolFile.CobolTree.ProcedureDivision.Sections;
            var alreadyShown = new List<string>();

            // for all top-level-sections
            foreach (var section in sections.Where(sec => !sec.IsReferencedBy.Any()))
            {
                text.AppendLine(section.Name);

                foreach (var procedure in section.Procedures)
                {
                    this.FindPerformsRecursively(text, procedure, "    ", alreadyShown);
                }

                text.AppendLine();
            }

            performTextBox.Text = text.ToString();
        }

        public void FindPerformsRecursively(StringBuilder text, Procedure procedure, string indent, List<string> alreadyShown)
        {
            if (procedure == null)
                return;

            foreach (var performReference in procedure.PerformReferences)
            {
                if (!alreadyShown.Contains(performReference.ReferenceName))
                {
                    text.AppendLine(indent + performReference.ReferenceName);
                    alreadyShown.Add(performReference.ReferenceName);
                    this.FindPerformsRecursively(text, performReference.Procedure, indent + "    ", alreadyShown);
                }
                else
                {
                    text.AppendLine(indent + performReference.ReferenceName + " (s.o.)");
                }
            }
        }
    }
}
