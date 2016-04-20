namespace Model
{
    using System.Collections.Generic;
    using System.Linq;

    public class DataDivision : Division
    {
        public List<Variable> Variables
        {
            get
            {
                return WorkingStorageSection.Variables.Union(LinkageSection.Variables).ToList();
            }
        }

        public DataDivision(string sourceCode, int indexDataDivision)
            : base(sourceCode, "Data Division", indexDataDivision)
        {
        }

        public WorkingStorageSection WorkingStorageSection { get; set; }

        public LinkageSection LinkageSection { get; set; }
    }
}
