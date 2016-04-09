using Logging;
using Model;
using Model.Pictures;
using System;
using System.Collections.Generic;

namespace Canal.Utils
{
    using System.Text.RegularExpressions;

    public static class VariablesUtil
    {
        public static List<Variable> AnalyzeVariables(string trimmedText)
        {
            // FILLER; REDEFINES; OCCURS; SPACES; 66
            // 	[ "USAGE" [ "IS" ] ] ( "BINARY" | "COMP" | "COMP-1" | "COMP-2" | "COMP-3" | "COMP-4" | "COMPUTATIONAL" | "COMPUTATIONAL-1" | "COMPUTATIONAL-2" | "COMPUTATIONAL-3" | "COMPUTATIONAL-4" | "DISPLAY" | "DISPLAY-1" | "INDEX" | "PACKED-DECIMAL" | "POINTER" )
            // ( "VALUE" [ "IS" ] | "VALUES" [ "ARE" ] ) { literal [ ( "THROUGH" | "THRU" ) literal ] }+
            // "RENAMES" qualified-data-name [ ( "THROUGH" | "THRU" ) qualified-data-name ]


            var result = new List<Variable>();
            var regex = new Regex(Constants.Variable, Constants.CompiledMultilineCaseInsensitive);

            Variable lastVariable = null;

            foreach (Match match in regex.Matches(trimmedText))
            {
                var level = int.Parse(match.Groups["level"].Value);
                var varName = match.Groups["token"].Value;
                var picture = ParsePicture(match.Groups["type"].Value);
                var currentVariable = new Variable(level, varName, picture, match.Value, null);

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

            return result;
        }

        private static IPic ParsePicture(string pictureText)
        {
            var trimmedText = pictureText.Trim().ToUpperInvariant();
            string picPart = string.Empty;
            string valuePart = string.Empty;

            try
            {
                // 1. extract pic and value parts: PIC X(03) VALUE "foo" => picPart = "X(03)", valuePart = ""foo""
                var indexOfPic = trimmedText.IndexOf("PIC", StringComparison.Ordinal);
                var indexOfValue = trimmedText.IndexOf("VALUE", StringComparison.Ordinal);

                if (indexOfValue < 0)
                {
                    if (indexOfPic < 0)
                        return null; // error condition
                    picPart = trimmedText.Substring(indexOfPic + 3).TrimStart();
                }
                else
                {
                    if (indexOfPic > -1)
                        picPart = trimmedText.Substring(indexOfPic + 3, indexOfValue - 1).Trim();
                    valuePart = trimmedText.Substring(indexOfValue + 5).TrimStart();
                }

                // 2. parse picPart: X(03) => XXX => new PicX(3), S9(2).99 => S99.99
                var result = !string.IsNullOrWhiteSpace(picPart) ? Pic.Parse(picPart) : new Pic88();

                // 3. parse valuePart: SPACES, ZEROS, "FOO", 42, 1.34
                if (!string.IsNullOrWhiteSpace(valuePart))
                    result.ParseValue(valuePart);

                return result;
            }
            catch (Exception exception)
            {
                Logger.Singleton.AddMsg(1, "Error parsing picture text \"{0}\": {1}: {2}", pictureText, exception.GetType().Name, exception.Message);
                return null;
            }
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
