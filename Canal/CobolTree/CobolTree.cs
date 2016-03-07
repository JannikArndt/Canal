namespace Canal.CobolTree
{
    using System;
    using System.Collections.Generic;
    using System.Windows.Forms;

    using Canal.Utils;

    public class CobolTree
    {
        public IdentificationDivision IdentificationDivision { get; set; }

        public EnvironmentDivision EnvironmentDivision { get; set; }

        public DataDivision DataDivision { get; set; }

        public ProcedureDivision ProcedureDivision { get; set; }

        public List<Variable> Variables
        {
            get
            {
                return this.DataDivision.Variables;
            }
        }

        private string _name;

        public TreeNode AsTreeNodes
        {
            get
            {
                var result = new TreeNode(this._name);
                if (this.IdentificationDivision != null)
                    result.Nodes.Add(this.IdentificationDivision);

                if (this.EnvironmentDivision != null)
                    result.Nodes.Add(this.EnvironmentDivision);

                if (this.DataDivision != null)
                    result.Nodes.Add(this.DataDivision);

                if (this.ProcedureDivision != null)
                    result.Nodes.Add(this.ProcedureDivision);
                return result;
            }
        }

        public CobolTree(string code, string name)
        {
            this._name = name;

            var sourceCode = TextUtil.TrimAllLines(code);

            int indexIdentificationDivision = sourceCode.IndexOf("IDENTIFICATION DIVISION", StringComparison.Ordinal);
            int indexEnvironmentDivision = sourceCode.IndexOf("ENVIRONMENT DIVISION", StringComparison.Ordinal);
            int indexDataDivision = sourceCode.IndexOf("DATA DIVISION", StringComparison.Ordinal);
            int indexProcedureDivision = sourceCode.IndexOf("PROCEDURE DIVISION", StringComparison.Ordinal);

            if (indexIdentificationDivision > 0)
                this.IdentificationDivision = new IdentificationDivision(sourceCode.Substring(indexProcedureDivision, indexEnvironmentDivision - indexIdentificationDivision), indexIdentificationDivision);

            if (indexEnvironmentDivision > 0)
                this.EnvironmentDivision = new EnvironmentDivision(sourceCode.Substring(indexEnvironmentDivision, indexDataDivision - indexEnvironmentDivision), indexEnvironmentDivision);

            if (indexDataDivision > 0)
                this.DataDivision = new DataDivision(sourceCode.Substring(indexDataDivision, indexProcedureDivision - indexDataDivision), indexDataDivision);

            if (indexProcedureDivision > 0)
                this.ProcedureDivision = new ProcedureDivision(sourceCode.Substring(indexProcedureDivision, sourceCode.Length - indexProcedureDivision), indexProcedureDivision);
        }
    }
}
