using Model;
using System.Collections.Generic;

namespace Canal.Utils
{
    using System.Text.RegularExpressions;

    public static class VariablesUtil
    {
        public static List<Variable> AnalyzeVariables(string text)
        {
            // FILLER; REDEFINES; OCCURS; SPACES; 66
            // 	[ "USAGE" [ "IS" ] ] ( "BINARY" | "COMP" | "COMP-1" | "COMP-2" | "COMP-3" | "COMP-4" | "COMPUTATIONAL" | "COMPUTATIONAL-1" | "COMPUTATIONAL-2" | "COMPUTATIONAL-3" | "COMPUTATIONAL-4" | "DISPLAY" | "DISPLAY-1" | "INDEX" | "PACKED-DECIMAL" | "POINTER" )
            // ( "VALUE" [ "IS" ] | "VALUES" [ "ARE" ] ) { literal [ ( "THROUGH" | "THRU" ) literal ] }+
            // "RENAMES" qualified-data-name [ ( "THROUGH" | "THRU" ) qualified-data-name ]


            var result = new List<Variable>();
            var regex =
                new Regex(
                    @"^[ ]+(?<level>\d\d) +(?<token>[\w\d-]+)( *)?(?<type>PIC [X()\d]+|VALUE "".+"")?\.?",
                    RegexOptions.Compiled | RegexOptions.IgnoreCase | RegexOptions.Multiline);

            Variable lastVariable = null;

            foreach (Match match in regex.Matches(text))
            {
                var level = int.Parse(match.Groups["level"].Value);
                var currentVariable = new Variable(level, match.Groups["token"].Value, match.Value, null);

                if (lastVariable == null || currentVariable.VariableLevel == 1 || currentVariable.VariableLevel == 77)
                {
                    result.Add(currentVariable);
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
                    while (currentVariable.VariableLevel <= lastVariable.VariableLevel && lastVariable != null)
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

            return result;
        }

        public static List<Literal> GetIdentifierLiterals(string text)
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
    }
}
