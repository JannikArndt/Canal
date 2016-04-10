using Model.Pictures;
using System.Collections.Generic;
using System.Windows.Forms;
// ReSharper disable MemberCanBePrivate.Global
// ReSharper disable AutoPropertyCanBeMadeGetOnly.Global
// ReSharper disable UnusedAutoPropertyAccessor.Global
// ReSharper disable UnusedAutoPropertyAccessor.Local
// ReSharper disable UseStringInterpolation
// ReSharper disable ConvertPropertyToExpressionBody

namespace Model
{
    public class Variable : TreeNode
    {
        public int VariableLevel { get; set; }

        public string VariableName { get; set; }

        public string Redefines { get; set; }

        public string Occurs { get; set; }

        public int Offset { get; set; }

        public int Length { get; set; }

        public IPic Picture { get; set; }

        private string Code { get; set; }

        public List<Variable> Variables { get; set; }

        public Variable ParentVariable { get; set; }

        public string VariableDefinition
        {
            get
            {
                return string.Format("{0}#{1}#{2}", VariableLevel.ToString("D2"), VariableName, Picture);
            }
        }

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

        public Variable() : base()
        {

        }

        public Variable(int variableLevel, string variableName, IPic picture, string code, Variable parentVariable)
                : base(variableLevel.ToString("D2") + "  " + variableName)
        {
            VariableLevel = variableLevel;
            VariableName = variableName;
            Picture = picture;
            Code = code;
            ParentVariable = parentVariable;
            Variables = new List<Variable>();
        }

        public override object Clone()
        {
            var clone = (Variable)base.Clone();
            clone.VariableLevel = VariableLevel;
            clone.VariableName = VariableName;
            clone.Picture = Picture;
            clone.Variables = Variables;
            clone.Length = Length;
            clone.Occurs = Occurs;
            clone.Offset = Offset;
            clone.Redefines = Redefines;
            clone.ParentVariable = ParentVariable;

            return clone;
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
