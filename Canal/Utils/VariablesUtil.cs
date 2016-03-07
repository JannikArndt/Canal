using System.Collections.Generic;

namespace Canal.Utils
{
    using System.Text.RegularExpressions;

    using Canal.CobolTree;

    public static class VariablesUtil
    {
        public static List<Variable> AnalyzeVariables(string text)
        {
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

                if (lastVariable == null || currentVariable.Level == 1 || currentVariable.Level == 77)
                {
                    result.Add(currentVariable);
                    lastVariable = currentVariable;
                }
                else if (currentVariable.Level > lastVariable.Level)
                {
                    currentVariable.Parent = lastVariable;
                    lastVariable.Variables.Add(currentVariable);
                    lastVariable = currentVariable;
                }
                else if (currentVariable.Level < lastVariable.Level || (currentVariable.Level != 88 && lastVariable.Level == 88))
                {
                    while (currentVariable.Level <= lastVariable.Level)
                        lastVariable = lastVariable.Parent;

                    currentVariable.Parent = lastVariable;
                    lastVariable.Variables.Add(currentVariable);
                    lastVariable = currentVariable;

                }
                else if (lastVariable.Level == currentVariable.Level)
                {
                    currentVariable.Parent = lastVariable.Parent;
                    lastVariable.Parent.Variables.Add(currentVariable);
                    lastVariable = currentVariable;
                }
            }

            return result;
        }

        public static List<string> GetAllVariables(CobolFile file)
        {
            return GetAllVariables(file.Text);
        }

        public static List<string> GetAllVariables(string text)
        {
            var result = new List<string>();
            var regex = new Regex(@" (?<token>[\w\d-]+)[\., ]", RegexOptions.Compiled | RegexOptions.IgnoreCase);
            foreach (Match match in regex.Matches(text))
            {
                var token = match.Groups["token"].Value;
                if (!Constants.cobolKeywords.Contains(token))
                    result.Add(token);
            }

            return result;
        }
    }
}
