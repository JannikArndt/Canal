using Model.File;

namespace Model.References
{
    public class ProcedureReference
    {
        protected ProcedureReference(string referencedProcedure)
        {
            ReferencedProcedure = referencedProcedure;
        }

        public ProcedureReference(Procedure procedure)
        {
            Procedure = procedure;
            ReferencedProcedure = procedure.Name;
        }

        public string ReferencedProcedure { get; set; }

        public Procedure Procedure { get; set; }
    }
}