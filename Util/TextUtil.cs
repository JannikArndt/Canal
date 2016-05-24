using Logging;
using Model;
using Model.References;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace Util
{
    public class TextUtil
    {
        public static readonly TextUtil Instance = new TextUtil();

        private TextUtil()
        {
        }

        public string TrimAllLines(string text)
        {
            var lines = text.Split(new[] { Environment.NewLine, "\n", "\r" }, StringSplitOptions.RemoveEmptyEntries);

            var result = new StringBuilder(lines.Length);

            foreach (var line in lines)
            {
                var length = Math.Min(66, line.Length - 6);
                if (length > 0)
                    result.AppendLine(line.Substring(6, length));
            }

            return result.ToString();
        }

        public DivisionAndSectionFlags FindDivisions(string text)
        {
            var result = new DivisionAndSectionFlags
            {
                Identification = new Regex("IDENTIFICATION DIVISION", Constants.CompiledCaseInsensitive).Match(text).Index,
                Environment = new Regex("ENVIRONMENT DIVISION", Constants.CompiledCaseInsensitive).Match(text).Index,
                Data = new Regex("DATA DIVISION", Constants.CompiledCaseInsensitive).Match(text).Index,
                Procedure = new Regex("PROCEDURE DIVISION", Constants.CompiledCaseInsensitive).Match(text).Index,
                WorkingStorage = new Regex("WORKING-STORAGE SECTION", Constants.CompiledCaseInsensitive).Match(text).Index,
                Linkage = new Regex("LINKAGE SECTION", Constants.CompiledCaseInsensitive).Match(text).Index
            };
            return result;
        }

        public IEnumerable<FileReference> FindCopyReferences(string text, bool textIsTrimmed = false)
        {
            Logger.Info("Finding copy references...");

            var prefix = textIsTrimmed ? @"^" : @"^.{6}";
            var copyRegex = new Regex(prefix + Constants.Copy, RegexOptions.Compiled | RegexOptions.IgnoreCase | RegexOptions.Multiline);

            var matches = copyRegex.Matches(text);
            Logger.Info("Found {0} COPYs...", matches.Count);

            return from Match match in matches.AsParallel()
                   select FileUtil.Instance.GetFileReference(match.Groups["program"].Value, match.Groups["folder"].Value);
        }

        public void Insert(CobolFile file, FileReference copyReference)
        {
            var newText = new StringBuilder();

            // Index of "COPY" in text might have changed, since other copys were resolved
            var copyRegex = new Regex(@"^.{6} +COPY +" + copyReference.ProgramName, RegexOptions.IgnoreCase | RegexOptions.Multiline);
            var updatedIndexOfCopy = copyRegex.Match(file.Text).Index;

            var lineBeforeCopy = file.Text.LastIndexOf(Environment.NewLine, updatedIndexOfCopy, StringComparison.Ordinal);
            var lineAfterCopy = file.Text.IndexOf(Environment.NewLine, updatedIndexOfCopy, StringComparison.Ordinal);

            newText.Append(file.Text.Substring(0, lineBeforeCopy + 8));
            newText.Append("*"); // Comment out COPY
            newText.Append(file.Text.Substring(lineBeforeCopy + 8, lineAfterCopy - (lineBeforeCopy + 8)).TrimEnd());
            newText.AppendLine("  *> COPY resolved by Canal");
            newText.Append(copyReference.CobolFile.Text + Environment.NewLine);
            newText.Append(file.Text.Substring(lineAfterCopy));

            file.Text = newText.ToString();
        }
    }
}
