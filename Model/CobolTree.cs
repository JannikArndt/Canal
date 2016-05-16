using Model.References;
using System.Collections.Generic;
using System.Linq;

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

        public List<FileReference> CallReferences
        {
            get
            {
                return ProcedureDivision.Sections.SelectMany(sec => sec.Procedures).SelectMany(proc => proc.CallReferences).ToList();
            }
        }

        private readonly string _name;

        public CobolTree(string name)
        {
            _name = name;
        }
    }
}
