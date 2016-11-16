using Model.Pictures;
using Model.References;
using System.Collections.Generic;

namespace Model.File
{
    /// <summary>
    /// Represents the declaration of a variable which can have children or a pic definition
    /// </summary>
    public class Variable
    {
        /// <summary>
        /// Level, i.e. 01, 03, 05, ...
        /// </summary>
        public int VariableLevel { get; private set; }

        /// <summary>
        /// Name
        /// </summary>
        public string VariableName { get; private set; }
        
        /// <summary>
        /// Type (PIC X, PIC 9, ...)
        /// </summary>
        public IPic Picture { get; private set; }

        /// <summary>
        /// Children, i.e. structures with a higher level defined on the same bytes
        /// </summary>
        public List<Variable> Variables { get; private set; }

        /// <summary>
        /// If this variable redefines another variable, it is referenced here
        /// </summary>
        public Variable Redefines { get; set; }

        /// <summary>
        /// How often this variable occurs (like an array)
        /// </summary>
        public int Occurs { get; set; }

        /// <summary>
        /// Byte-Offset to root
        /// </summary>
        public int Offset { get; set; }

        /// <summary>
        /// Length in bytes
        /// </summary>
        public int ByteLength { get; set; }
        
        /// <summary>
        /// Reference to parent or null if this is the root
        /// </summary>
        public Variable ParentVariable { get; set; }

        /// <summary>
        /// Reference to the copy book where this is defined
        /// </summary>
        public FileReference CopyReference { get; set; }

        /// <summary>
        /// Character at which the variable declaration starts (for sorting)
        /// </summary>
        public int Index { get; set; }

        /// <summary>
        /// String-representation with level, name and PIC
        /// </summary>
        public string VariableDefinition
        {
            get
            {
                return string.Format("{0:D2} {1} {2}", VariableLevel, VariableName, Picture);
            }
        }

        /// <summary>
        /// Root variable, i.e. 01 or 77 structure
        /// </summary>
        public Variable Root
        {
            get
            {
                if (ParentVariable == null) return VariableLevel == 1 ? this : null;

                var result = ParentVariable;
                while ((result.VariableLevel != 1 || result.VariableLevel != 77) && result.ParentVariable != null)
                    result = result.ParentVariable;
                return result;
            }
        }

        public Variable(int variableLevel, string variableName, IPic picture, Variable parentVariable)
        {
            VariableLevel = variableLevel;
            VariableName = variableName;
            Picture = picture;
            ParentVariable = parentVariable;
            Variables = new List<Variable>();
        }

        /// <summary>
        /// Returns a string representation of the Level and Name
        /// </summary>
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
