namespace Canal.CobolTree
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