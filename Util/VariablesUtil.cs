using Model;
using Model.Enums;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using System.Windows.Forms;

namespace Util
{
    public class VariablesUtil
    {
        public static readonly VariablesUtil Instance = new VariablesUtil();

        private VariablesUtil()
        {
        }

        public ConcurrentDictionary<string, Variable> AnalyzeVariables(CobolFile cobolFile)
        {
            // FILLER; REDEFINES; OCCURS; SPACES; 66
            // [ "USAGE" [ "IS" ] ] ( "BINARY" | "COMP" | "COMP-1" | "COMP-2" | "COMP-3" | "COMP-4" | "COMPUTATIONAL" | "COMPUTATIONAL-1" | "COMPUTATIONAL-2" | "COMPUTATIONAL-3" | "COMPUTATIONAL-4" | "DISPLAY" | "DISPLAY-1" | "INDEX" | "PACKED-DECIMAL" | "POINTER" )
            // ( "VALUE" [ "IS" ] | "VALUES" [ "ARE" ] ) { literal [ ( "THROUGH" | "THRU" ) literal ] }+
            // "RENAMES" qualified-data-name [ ( "THROUGH" | "THRU" ) qualified-data-name ]

            var trimmedText = TextUtil.Instance.TrimAllLines(cobolFile.Text);

            var result = new ConcurrentDictionary<string, Variable>();
            var regex = new Regex(Constants.Variable, Constants.CompiledMultilineCaseInsensitive);
            var preparedText = trimmedText.Replace("\t", " ");
            Variable lastVariable = null;

            foreach (Match match in regex.Matches(preparedText))
            {
                // Read properties from RegEx
                var valLevel = int.Parse(match.Groups["level"].Value);
                var valLiteral = match.Groups["literal"].Value;
                var valRedefines = match.Groups["redefines"].Value;
                var valType = match.Groups["type"].Value;
                var valComp = match.Groups["comp"].Value;
                var valValue = match.Groups["value"].Value;
                var valOccurs = match.Groups["occurs"].Value;

                // Create type definition ("PIC")
                var picture = PicParser.Instance.ParsePicture(valType, valComp, valValue, valLevel);

                // Find redefined variables
                Variable redefined = null;
                if (!string.IsNullOrWhiteSpace(valRedefines))
                {
                    if (!result.ContainsKey(valRedefines))
                        Logging.Logger.Error("Redefined variable {0} not found in file {1} (variable definition: {2}).", valRedefines, cobolFile.Name, match.Value);
                    else
                        redefined = result[valRedefines];
                }

                if (valLiteral == "FILLER")
                    Console.WriteLine("Foo");

                // Create Variable
                var currentVariable = new Variable(valLevel, valLiteral, picture, match.Value, null)
                {
                    Redefines = redefined,
                    Occurs = !string.IsNullOrWhiteSpace(valOccurs) ? int.Parse(valOccurs) : 1,
                    CopyReference = cobolFile.FileReference
                };

                // Save result to dictionary
                result.TryAdd(currentVariable.VariableName, currentVariable);

                // Create references between variables
                if (lastVariable == null || currentVariable.VariableLevel == 1 || currentVariable.VariableLevel == 77)
                {
                    lastVariable = currentVariable;
                }
                else if (currentVariable.VariableLevel > lastVariable.VariableLevel)
                {
                    currentVariable.ParentVariable = lastVariable;
                    lastVariable.Variables.Add(currentVariable);
                    lastVariable = currentVariable;
                }
                else if (currentVariable.VariableLevel < lastVariable.VariableLevel || (currentVariable.VariableLevel != 88 && lastVariable.VariableLevel == 88))
                {
                    while (lastVariable != null && lastVariable.ParentVariable != null && currentVariable.VariableLevel <= lastVariable.VariableLevel)
                        lastVariable = lastVariable.ParentVariable;

                    currentVariable.ParentVariable = lastVariable;
                    lastVariable.Variables.Add(currentVariable);
                    lastVariable = currentVariable;
                }
                else if (lastVariable.VariableLevel == currentVariable.VariableLevel)
                {
                    currentVariable.ParentVariable = lastVariable.ParentVariable;
                    if (lastVariable.ParentVariable != null)
                        lastVariable.ParentVariable.Variables.Add(currentVariable);
                    lastVariable = currentVariable;
                }
            }

            foreach (var variable in result.Values.Where(vari => vari.VariableLevel == 1))
            {
                variable.ByteLength = GetByteLength(variable, 0);
            }

            return result;
        }

        private int GetByteLength(Variable variable, int currentOffset)
        {
            // redefines
            if (variable.Redefines != null)
                currentOffset = variable.Redefines.Offset;

            variable.Offset = currentOffset;

            var childrenByteLength = 0;

            foreach (var child in variable.Variables)
            {
                childrenByteLength += GetByteLength(child, variable.Offset + childrenByteLength);
            }

            variable.ByteLength = variable.Picture.ByteLength + childrenByteLength;

            if (variable.Occurs > 1)
                variable.ByteLength = variable.ByteLength * variable.Occurs;

            // redefinitions' lengths do not count
            if (variable.Redefines != null)
                return 0;

            return variable.ByteLength;
        }

        public IEnumerable<Literal> GetIdentifierLiterals(string text)
        {
            var result = new List<Literal>();
            var regex = new Regex(Constants.LiteralWithInputOutput, Constants.CompiledMultilineCaseInsensitive); // @" (?<token>[\w\d-]+)[\., ]"

            foreach (Match match in regex.Matches(text))
            {
                var literal = match.Groups["literal"].Value;
                var usedAs = UsedAs.Unknown;
                if (match.Groups["beforeOutput"].Success)
                    usedAs = UsedAs.Output;

                if (match.Groups["beforeInputWithSpace"].Success || match.Groups["beforeInput"].Success)
                {
                    usedAs = usedAs == UsedAs.Output ? UsedAs.Both : UsedAs.Input;
                }

                if (!Constants.CobolKeywords.Contains(literal))
                    result.Add(new Literal(literal, usedAs));
            }

            return result;
        }

        public TreeNode ConvertToTreeNode(Variable variable)
        {
            var result = new TreeNode(variable.GetLevelAndName()) { Tag = variable };

            foreach (var child in variable.Variables)
            {
                result.Nodes.Add(ConvertToTreeNode(child));
            }

            return result;
        }
    }
}
