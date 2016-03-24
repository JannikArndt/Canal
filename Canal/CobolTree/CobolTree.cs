namespace Canal.CobolTree
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Windows.Forms;

    using Utils;

    public class CobolTree
    {
        public IdentificationDivision IdentificationDivision { get; set; }

        public EnvironmentDivision EnvironmentDivision { get; set; }

        public DataDivision DataDivision { get; set; }

        public ProcedureDivision ProcedureDivision { get; set; }

        public List<Division> Divisions { get { return new List<Division> { IdentificationDivision, EnvironmentDivision, DataDivision, ProcedureDivision }; } }

        public List<Variable> Variables { get { return DataDivision.Variables; } }

        public List<FileReference> CallReferences
        {
            get
            {
                return ProcedureDivision.Sections.SelectMany(sec => sec.Procedures).SelectMany(proc => proc.CallReferences).ToList();
            }
        }

        public string Name { get; private set; }

        public int LinesOfCode { get { return Divisions.Sum(div => div.LinesOfCode); } }

        public TreeNode AsTreeNodes
        {
            get
            {
                var result = new TreeNode(Name);
                if (IdentificationDivision != null)
                    result.Nodes.Add(IdentificationDivision);

                if (EnvironmentDivision != null)
                    result.Nodes.Add(EnvironmentDivision);

                if (DataDivision != null)
                    result.Nodes.Add(DataDivision);

                if (ProcedureDivision != null)
                    result.Nodes.Add(ProcedureDivision);
                return result;
            }
        }

        public CobolTree(string code, string name)
        {
            Name = name;

            var sourceCode = TextUtil.TrimAllLines(code);

            int indexIdentificationDivision = sourceCode.IndexOf("IDENTIFICATION DIVISION", StringComparison.Ordinal);
            int indexEnvironmentDivision = sourceCode.IndexOf("ENVIRONMENT DIVISION", StringComparison.Ordinal);
            int indexDataDivision = sourceCode.IndexOf("DATA DIVISION", StringComparison.Ordinal);
            int indexProcedureDivision = sourceCode.IndexOf("PROCEDURE DIVISION", StringComparison.Ordinal);

            IdentificationDivision = indexIdentificationDivision > 0
                ? new IdentificationDivision(sourceCode.Substring(indexProcedureDivision, Math.Max(0, indexEnvironmentDivision - indexIdentificationDivision)), indexIdentificationDivision)
                : new IdentificationDivision("", 0);

            EnvironmentDivision = indexEnvironmentDivision > 0
                ? new EnvironmentDivision(sourceCode.Substring(indexEnvironmentDivision, Math.Max(0, indexDataDivision - indexEnvironmentDivision)), indexEnvironmentDivision)
                : new EnvironmentDivision("", 0);

            DataDivision = indexDataDivision > 0
                ? new DataDivision(sourceCode.Substring(indexDataDivision, Math.Max(0, indexProcedureDivision - indexDataDivision)), indexDataDivision)
                : new DataDivision("", 0);

            ProcedureDivision = indexProcedureDivision > 0
                ? new ProcedureDivision(sourceCode.Substring(indexProcedureDivision, Math.Max(0, sourceCode.Length - indexProcedureDivision)), indexProcedureDivision)
                : new ProcedureDivision("", 0);

            var allProcedures = ProcedureDivision.Sections.SelectMany(s => s.Procedures).ToList();
            foreach (var procedure in allProcedures)
            {
                procedure.AnalyzeVariables(DataDivision.Variables);
                procedure.AnalyzePerformReferences();
                procedure.AnalyzeGoTos();
                procedure.AnalyzeCalls();
            }

            // fix deeper references
            foreach (var performReference in allProcedures.SelectMany(procedure => procedure.PerformReferences.Where(pref => pref.Procedure == null)))
            {
                performReference.Procedure = allProcedures.FirstOrDefault(p => p.Name == performReference.ReferencedProcedure);
            }

        }
    }
}
