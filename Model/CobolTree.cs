using Model.References;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;

namespace Model
{
    public class CobolTree
    {
        public IdentificationDivision IdentificationDivision { get; set; }

        public EnvironmentDivision EnvironmentDivision { get; set; }

        public DataDivision DataDivision { get; set; }

        public ProcedureDivision ProcedureDivision { get; set; }

        public IEnumerable<Procedure> AllProcedures
        {
            get
            {
                return ProcedureDivision.Sections.SelectMany(s => s.Procedures).ToList();
            }
        }

        public List<Division> Divisions { get { return new List<Division> { IdentificationDivision, EnvironmentDivision, DataDivision, ProcedureDivision }; } }

        public List<Variable> Variables { get { return DataDivision.Variables; } }

        public List<FileReference> CallReferences
        {
            get
            {
                return ProcedureDivision.Sections.SelectMany(sec => sec.Procedures).SelectMany(proc => proc.CallReferences).ToList();
            }
        }

        public int LinesOfCode { get { return Divisions.Sum(div => div.LinesOfCode); } }

        private readonly string _name;

        public CobolTree(string name)
        {
            _name = name;
        }

        public TreeNode GetAsTreeNodes()
        {
            var result = new TreeNode(_name);
            if (IdentificationDivision != null)
                result.Nodes.Add(IdentificationDivision);

            if (EnvironmentDivision != null)
                result.Nodes.Add(EnvironmentDivision);

            if (DataDivision != null)
                result.Nodes.Add(DataDivision);

            if (ProcedureDivision != null)
                result.Nodes.Add(ProcedureDivision);
            return result;
        }
    }
}
