using System.Collections.Generic;
using System.Linq;
using Model.References;

namespace Model.File
{
    /// <summary>
    /// Semantic view of a cobol file
    /// </summary>
    public class CobolTree
    {
        /// <summary>
        /// The IDENTIFICATION DIVISION
        /// </summary>
        public IdentificationDivision IdentificationDivision { get; set; }

        /// <summary>
        /// The ENVIRONMENT DIVISION
        /// </summary>
        public EnvironmentDivision EnvironmentDivision { get; set; }

        /// <summary>
        /// The DATA DIVISION
        /// </summary>
        public DataDivision DataDivision { get; set; }

        /// <summary>
        /// The PROCEDURE DIVISION
        /// </summary>
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
