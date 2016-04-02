using Model;
using System;
using System.ComponentModel;

namespace CodeGenerator
{
    public class GeneratorModel : IMappingProvider
    {
        [DisplayName("Map?")]
        public bool DoMap { get; set; }

        [DisplayName("Cobol Variable Name")]
        public string CobolVariableName
        {
            get { return Variable.VariableName; }
        }

        [DisplayName("Variable Type")]
        public CobolVariableTypes VariableType { get; set; }

        public Variable Variable { get; set; }

        [DisplayName("Generated Property Name")]
        public string PropertyName { get; set; }

        [DisplayName("Generated Property Type")]
        public GeneratedCodeTypes GeneratedCodeType { get; set; }

        [DisplayName("Use Mapper")]
        public string MapperName { get; set; }

        [DisplayName("Comment")]
        public string Comment { get; set; }

        /// <summary>
        /// Creates a new instance of the GeneratorModel class.
        /// </summary>
        /// <param name="variable"></param>
        /// <param name="variableType"></param>
        /// <param name="propertyName"></param>
        /// <param name="generatedCodeType"></param>
        /// <param name="comment"></param>
        public GeneratorModel(Variable variable, CobolVariableTypes variableType, string propertyName, GeneratedCodeTypes generatedCodeType, string comment = null)
        {
            Variable = variable;
            DoMap = true;
            VariableType = variableType;
            PropertyName = propertyName;
            GeneratedCodeType = generatedCodeType;
            MapperName = string.Empty;
            Comment = comment ?? "[" + variable.VariableName + "]";
        }

        /// <summary>
        /// Creates the code for a property definition, similar to <para />
        /// <code>&lt;summary&gt;My String [MY-TEXT]&lt;/summary&gt;<para />public String MyText { get; set; }</code>
        /// </summary>
        /// <returns>A formatted string</returns>
        public string GetPropertyDefinition()
        {
            return "        /// <summary>\n        /// {Comment}\n        /// [{CobolVariableName}]\n        /// </summary>\n        public {GeneratedCodeType} {PropertyName} {{ get; set; }}"
                .FormatWith(new { Comment, CobolVariableName, GeneratedCodeType, PropertyName });

        }

        /// <summary>
        /// <para>Creates the code for mapping a byte array to a property, similar to</para>
        /// <code> MyText = bytes.GetString(0, 10)</code>
        /// </summary>
        /// <returns>A formatted string</returns>
        public string GetMappingFromCobol()
        {
            var commentText = string.IsNullOrWhiteSpace(Comment) ? string.Empty : "                 // " + Comment.Replace("\n", " / ") + Environment.NewLine;

            switch (GeneratedCodeType)
            {
                case GeneratedCodeTypes.String:
                    return "{Comment}                 {PropertyName} = bytes.GetString({Offset}, {Length})"
                        .FormatWith(new { Comment = commentText, PropertyName, Variable.Offset, Variable.Length });
                default:
                    return "{Comment}                 {PropertyName} = bytes.GetBytes({Offset}, {Length})"
                         .FormatWith(new { Comment = commentText, PropertyName, Variable.Offset, Variable.Length });
            }
        }

        /// <summary>
        /// <para>Creates the code for mapping a property to a byte array, similar to</para>
        /// <code>bytes.SetString(0, 10, myBusinessObject.MyText)</code>
        /// </summary>
        /// <param name="objectName">The name ob the object the property belongs to</param>
        /// <returns>A formatted string</returns>
        public string GetMappingToCobol(string objectName)
        {
            var commentText = string.IsNullOrWhiteSpace(Comment) ? string.Empty : "            // " + Comment.Replace("\n", " / ") + Environment.NewLine;

            switch (GeneratedCodeType)
            {
                case GeneratedCodeTypes.String:
                    return "{Comment}            bytes.SetString({Offset}, {Length}, {ObjectName}.{PropertyName})"
                        .FormatWith(new { Comment = commentText, Variable.Offset, Variable.Length, ObjectName = objectName, PropertyName });
                default:
                    return "            // TODO No mapping found! Using bytes-mapping:" + Environment.NewLine
                        + "{Comment}            bytes.SetBytes({Offset}, {Length}, {ObjectName}.{PropertyName})"
                        .FormatWith(new { Comment = commentText, Variable.Offset, Variable.Length, ObjectName = objectName, PropertyName });
            }
        }
    }
}
