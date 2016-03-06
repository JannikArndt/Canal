using System;
using System.Text.RegularExpressions;

namespace Canal
{
    public static class ReferenceUtil
    {
        public static void ResolveCopys(CobolFile file)
        {
            var copyRegex = new Regex(@"^[\d ]{7}COPY (?<program>[\w]+)( OF (?<folder>[\w]+)\.)", RegexOptions.Compiled | RegexOptions.IgnoreCase | RegexOptions.Multiline);

            foreach (Match match in copyRegex.Matches(file.Text))
            {
                var copyFile = FileUtil.Get(match.Groups["program"].Value, match.Groups["folder"].Value);

                if (copyFile == null)
                    continue;

                var lineAfterCopy = file.Text.IndexOf(Environment.NewLine, match.Index, StringComparison.Ordinal);
                file.Text = file.Text.Insert(lineAfterCopy + 1, copyFile.Text + Environment.NewLine);
            }
        }
    }
}
