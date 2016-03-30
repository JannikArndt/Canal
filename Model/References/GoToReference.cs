namespace Model.References
{
    public class GoToReference : ProcedureReference
    {
        public GoToReference(string referencedProcedure) : base(referencedProcedure)
        {
        }

        public override string ToString()
        {
            return string.Format("GO TO {0}", ReferencedProcedure);
        }
    }
}
