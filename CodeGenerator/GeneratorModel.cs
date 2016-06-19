using Model;
using Model.Pictures;
using System;
using Model.File;

// ReSharper disable LocalizableElement
// ReSharper disable MemberCanBePrivate.Global
// ReSharper disable ConvertPropertyToExpressionBody

namespace CodeGenerator
{
    public class GeneratorModel : IMappingProvider
    {
        public bool DoMap { get; set; }

        public string Level
        {
            get
            {
                var indent = Variable.VariableLevel == 88
                    ? 10
                    : Variable.VariableLevel == 77
                        ? 0
                        : (Variable.VariableLevel - 1) * 2;
                return new string(' ', indent) + Variable.VariableLevel.ToString("D2");
            }
        }

        public string CobolVariableName { get { return Variable.VariableName; } }

        public string VariableType { get { return Variable.Picture.GetType().Name; } }

        public int Offset { get { return Variable.Offset; } }

        public string Bytes
        {
            get
            {
                return Variable.Picture is PicGroup || Variable.Redefines != null
                        ? "(" + Variable.ByteLength + ")"
                        : Variable.ByteLength.ToString();
            }
        }

        public Variable Variable { get; set; }

        public string PropertyName { get; set; }

        public GeneratedCodeTypes GeneratedCodeType { get; set; }

        public string MapperName { get; set; }

        public string Comment { get; set; }

        public GeneratorModel()
        {

        }

        /// <summary>
        /// Creates a new instance of the GeneratorModel class.
        /// </summary>
        /// <param name="variable"></param>
        /// <param name="propertyName"></param>
        /// <param name="generatedCodeType"></param>
        /// <param name="comment"></param>
        public GeneratorModel(Variable variable, string propertyName, GeneratedCodeTypes generatedCodeType, string comment = null)
        {
            Variable = variable;
            DoMap = true;
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
                case GeneratedCodeTypes.@string:
                    return "{Comment}                 {PropertyName} = bytes.GetString({Offset}, {ByteLength})"
                        .FormatWith(new { Comment = commentText, PropertyName, Variable.Offset, Variable.Picture.ByteLength });
                default:
                    return "{Comment}                 {PropertyName} = bytes.GetBytes({Offset}, {ByteLength})"
                         .FormatWith(new { Comment = commentText, PropertyName, Variable.Offset, Variable.Picture.ByteLength });
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
                case GeneratedCodeTypes.@string:
                    return "{Comment}            bytes.SetString({Offset}, {ByteLength}, {ObjectName}.{PropertyName})"
                        .FormatWith(new { Comment = commentText, Variable.Offset, Variable.Picture.ByteLength, ObjectName = objectName, PropertyName });
                default:
                    return "            // TODO No mapping found! Using bytes-mapping:" + Environment.NewLine
                        + "{Comment}            bytes.SetBytes({Offset}, {ByteLength}, {ObjectName}.{PropertyName})"
                        .FormatWith(new { Comment = commentText, Variable.Offset, Variable.Picture.ByteLength, ObjectName = objectName, PropertyName });
            }
        }
    }
}
