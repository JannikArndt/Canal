namespace Canal.CobolTree.Models
{
    using System;

    public class CobolTree
    {
        public IdentificationDivision IdentificationDivision { get; set; }

        public EnvironmentDivision EnvironmentDivision { get; set; }

        public DataDivision DataDivision { get; set; }

        public ProcedureDivision ProcedureDivision { get; set; }

        public CobolTree(string sourceCode)
        {
            int indexIdentificationDivision = sourceCode.IndexOf("IDENTIFICATION DIVISION", StringComparison.Ordinal);
            int indexEnvironmentDivision = sourceCode.IndexOf("ENVIRONMENT DIVISION", StringComparison.Ordinal);
            int indexDataDivision = sourceCode.IndexOf("DATA DIVISION", StringComparison.Ordinal);
            int indexProcedureDivision = sourceCode.IndexOf("PROCEDURE DIVISION", StringComparison.Ordinal);

            this.IdentificationDivision = new IdentificationDivision(sourceCode.Substring(indexProcedureDivision, indexEnvironmentDivision - indexIdentificationDivision));

            this.EnvironmentDivision = new EnvironmentDivision(sourceCode.Substring(indexEnvironmentDivision, indexDataDivision - indexEnvironmentDivision));

            this.DataDivision = new DataDivision(sourceCode.Substring(indexDataDivision, indexProcedureDivision - indexDataDivision));

            this.ProcedureDivision = new ProcedureDivision(sourceCode.Substring(indexProcedureDivision, sourceCode.Length - indexProcedureDivision));
        }
    }
}
