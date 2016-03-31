
using System.Collections.Generic;
using System.Globalization;
using System.Windows.Forms;

namespace Model
{
    public class Variable : TreeNode
    {
        public int VariableLevel { get; set; }

        public string VariableName { get; set; }

        // ReSharper disable once UnusedAutoPropertyAccessor.Local
        private string Code { get; set; }

        public List<Variable> Variables { get; set; }

        public Variable ParentVariable { get; set; }

        public Variable Root
        {
            get
            {
                if (ParentVariable == null) return null;

                var result = ParentVariable;
                while ((result.VariableLevel != 1 || result.VariableLevel != 77) && result.ParentVariable != null)
                    result = result.ParentVariable;
                return result;
            }
        }

        public Variable(int variableLevel, string variableName, string code, Variable parentVariable)
            : base(variableLevel.ToString(CultureInfo.InvariantCulture).PadLeft(2, '0') + "  " + variableName)
        {
            VariableLevel = variableLevel;
            VariableName = variableName;
            Code = code;
            ParentVariable = parentVariable;
            Variables = new List<Variable>();
        }

        public void FillNodesWithVariables()
        {
            Nodes.Clear();

            foreach (var variable in Variables)
            {
                Nodes.Add(variable);

                variable.FillNodesWithVariables();
            }
        }

        public override string ToString()
        {
            return string.Format("{0} {1}, Variables: {2}", VariableLevel, VariableName, Variables.Count);
        }
    }
}
