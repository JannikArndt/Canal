using System.Collections.Generic;
using System.Linq;

namespace Model
{
    public class Section : Procedure
    {
        public List<Procedure> Procedures { get; private set; }

        public new int LinesOfCode { get { return Procedures.Sum(proc => proc.LinesOfCode); } }

        public Section(string name, int indexInSourceCode)
            : base(name, indexInSourceCode)
        {
            Procedures = new List<Procedure>();
        }
    }
}
