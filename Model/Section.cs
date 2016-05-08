using System.Collections.Generic;

namespace Model
{
    public class Section : Procedure
    {
        public List<Procedure> Procedures { get; private set; }

        public Section(CobolFile cobolFile, string name, int startIndex, int endIndex)
            : base(cobolFile, name, startIndex, endIndex)
        {
            Procedures = new List<Procedure>();
        }

        public void AddProcedure(Procedure procedure)
        {
            Procedures.Add(procedure);
            Nodes.Add(procedure);
        }
    }
}
