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

        public IEnumerable<Division> GetAllDivisions()
        {
            return new List<Division> { IdentificationDivision, EnvironmentDivision, DataDivision, ProcedureDivision };
        }

        public IEnumerable<Section> GetAllSections()
        {
            return ProcedureDivision.Sections.Union(new Section[] { DataDivision.WorkingStorageSection, DataDivision.LinkageSection });
        }

        public IEnumerable<Procedure> GetAllProcedures()
        {
            return ProcedureDivision.Sections.SelectMany(s => s.Procedures);
        }

        public IEnumerable<FileReference> CallReferences
        {
            get
            {
                return ProcedureDivision.Sections.SelectMany(sec => sec.Procedures).SelectMany(proc => proc.CallReferences).ToList();
            }
        }
    }
}
