using System.Windows.Forms;

namespace Canal.CobolTree.Models
{
    using System;

    public class CobolTree
    {
        public IdentificationDivision IdentificationDivision { get; set; }

        public EnvironmentDivision EnvironmentDivision { get; set; }

        public DataDivision DataDivision { get; set; }

        public ProcedureDivision ProcedureDivision { get; set; }

        private string _name;

        public TreeNode[] AsTreeNodes
        {
            get
            {
                return new[]
                {
                    new TreeNode(_name, new TreeNode[] {
                    IdentificationDivision,
                    EnvironmentDivision,
                    DataDivision,
                    ProcedureDivision})
                };
            }


        }

        public CobolTree(string code, string name)
        {
            _name = name;

            var sourceCode = TextHelper.TrimAllLines(code);

            int indexIdentificationDivision = sourceCode.IndexOf("IDENTIFICATION DIVISION", StringComparison.Ordinal);
            int indexEnvironmentDivision = sourceCode.IndexOf("ENVIRONMENT DIVISION", StringComparison.Ordinal);
            int indexDataDivision = sourceCode.IndexOf("DATA DIVISION", StringComparison.Ordinal);
            int indexProcedureDivision = sourceCode.IndexOf("PROCEDURE DIVISION", StringComparison.Ordinal);

            if (indexIdentificationDivision > 0)
                IdentificationDivision = new IdentificationDivision(sourceCode.Substring(indexProcedureDivision, indexEnvironmentDivision - indexIdentificationDivision));

            if (indexEnvironmentDivision > 0)
                EnvironmentDivision = new EnvironmentDivision(sourceCode.Substring(indexEnvironmentDivision, indexDataDivision - indexEnvironmentDivision));

            if (indexDataDivision > 0)
                DataDivision = new DataDivision(sourceCode.Substring(indexDataDivision, indexProcedureDivision - indexDataDivision));

            if (indexProcedureDivision > 0)
                ProcedureDivision = new ProcedureDivision(sourceCode.Substring(indexProcedureDivision, sourceCode.Length - indexProcedureDivision));
        }
    }
}
