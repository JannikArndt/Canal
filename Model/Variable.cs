using Model.Pictures;
using Model.References;
using System.Collections.Generic;

namespace Model
{
    public class Variable
    {
        public int VariableLevel { get; set; }

        public string VariableName { get; set; }

        public Variable Redefines { get; set; }

        public int Occurs { get; set; }

        public int Offset { get; set; }

        public int ByteLength { get; set; }

        public IPic Picture { get; set; }

        private string Code { get; set; }

        public List<Variable> Variables { get; set; }

        public Variable ParentVariable { get; set; }

        public FileReference CopyReference { get; set; }

        public string VariableDefinition
        {
            get
            {
                return string.Format("{0} {1} {2}", VariableLevel.ToString("D2"), VariableName, Picture);
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

        public Variable()
        {
        }

        public Variable(int variableLevel, string variableName, IPic picture, string code, Variable parentVariable)
        {
            VariableLevel = variableLevel;
            VariableName = variableName;
            Picture = picture;
            Code = code;
            ParentVariable = parentVariable;
            Variables = new List<Variable>();
        }

        public string GetLevelAndName()
        {
            return VariableLevel.ToString("D2") + "  " + VariableName;
        }

        public override string ToString()
        {
            return string.Format("{0} {1}, Variables: {2}", VariableLevel, VariableName, Variables.Count);
        }
    }
}
