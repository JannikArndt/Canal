using Model.Enums;
using Model.File;

namespace Model.References
{
    public class PerformReference : ProcedureReference
    {
        public PerformReference(string referencedProcedure) : base(referencedProcedure)
        {
        }

        public PerformReference(Procedure procedure) : base(procedure)
        {
        }

        public PerformType Type { get; set; }

        public override string ToString()
        {
            return string.Format("PERFORM {0}", ReferencedProcedure);
        }
    }
}
