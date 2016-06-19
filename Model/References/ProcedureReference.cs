using Model.File;

namespace Model.References
{
    public class ProcedureReference
    {
        protected ProcedureReference(string referencedProcedure)
        {
            ReferencedProcedure = referencedProcedure;
        }

        public string ReferencedProcedure { get; set; }

        public Procedure Procedure { get; set; }
    }
}