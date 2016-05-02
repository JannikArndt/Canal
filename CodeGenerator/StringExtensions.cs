using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Web.UI;

namespace CodeGenerator
{
    public static class StringExtensions
    {
        /// <summary>
        /// Extension method to format a string with named parameters. Written by the great James Newton-King
        /// (http://james.newtonking.com/archive/2008/03/29/formatwith-2-0-string-formatting-with-named-variables)
        /// <example>
        /// Status.Text = "{UserName} last logged in at {LastLoginDate}".FormatWith(user);
        /// </example>
        /// <example>
        /// "{CurrentTime} - {ProcessName}".FormatWith(new { CurrentTime = DateTime.Now, ProcessName = p.ProcessName });
        /// </example>
        /// </summary>
        /// <param name="format">The string to format</param>
        /// <param name="source">The object whose values will be used</param>
        /// <returns>A formatted string</returns>
        public static string FormatWith(this string format, object source)
        {
            return FormatWith(format, null, source);
        }

        /// <summary>
        /// Extension method to format a string with named parameters. Written by the great James Newton-King
        /// (http://james.newtonking.com/archive/2008/03/29/formatwith-2-0-string-formatting-with-named-variables)
        /// <example>
        /// Status.Text = "{UserName} last logged in at {LastLoginDate}".FormatWith(user);
        /// </example>
        /// <example>
        /// "{CurrentTime} - {ProcessName}".FormatWith(new { CurrentTime = DateTime.Now, ProcessName = p.ProcessName });
        /// </example>
        /// </summary>
        /// <param name="format">The string to format</param>
        /// <param name="provider">An IFormatProvider or null</param>
        /// <param name="source">The object whose values will be used</param>
        /// <returns>A formatted string</returns>
        public static string FormatWith(this string format, IFormatProvider provider, object source)
        {
            if (format == null)
                throw new ArgumentNullException("format");

            Regex r = new Regex(@"(?<start>\{)+(?<property>[\w\.\[\]]+)(?<format>:[^}]+)?(?<end>\})+",
              RegexOptions.Compiled | RegexOptions.CultureInvariant | RegexOptions.IgnoreCase);

            List<object> values = new List<object>();
            string rewrittenFormat = r.Replace(format, delegate (Match m)
            {
                Group startGroup = m.Groups["start"];
                Group propertyGroup = m.Groups["property"];
                Group formatGroup = m.Groups["format"];
                Group endGroup = m.Groups["end"];

                values.Add((propertyGroup.Value == "0")
                  ? source
                  : DataBinder.Eval(source, propertyGroup.Value));

                return new string('{', startGroup.Captures.Count) + (values.Count - 1) + formatGroup.Value
                  + new string('}', endGroup.Captures.Count);
            });

            return string.Format(provider, rewrittenFormat, values.ToArray());
        }

        public static int GetIndexOfCurrentLineStart(this string text, int index)
        {
            // if index is line break => go backwards to first actual character
            while (text[index] == '\n' || text[index] == '\r')
            {
                index--;
            }

            // look for last line break
            while (index > -1)
            {
                if (text[index] == '\n' || text[index] == '\r')
                {
                    return index + 1;
                }
                index--;
            }

            // else this is the first line
            return 0;
        }

        public static int GetIndexOfPreviousLineStart(this string text, int index)
        {
            var startOfCurrentLine = text.GetIndexOfCurrentLineStart(index);
            return startOfCurrentLine == 0 ? 0 : text.GetIndexOfCurrentLineStart(startOfCurrentLine - 1);
        }

        public static bool IsCommentLine(this string text, int index)
        {
            var startOfLine = text.GetIndexOfCurrentLineStart(index);
            return text[startOfLine + 6] == '*' || (text[startOfLine + 6] == '/' && text[startOfLine + 7] == '*');
        }
    }
}
