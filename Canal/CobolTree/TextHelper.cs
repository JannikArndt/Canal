using System;
using System.Text;

namespace Canal.CobolTree
{
    public static class TextHelper
    {
        public static string TrimAllLines(string text)
        {
            var lines = text.Split(new[] { Environment.NewLine, "\n", "\r" }, StringSplitOptions.RemoveEmptyEntries);

            var result = new StringBuilder(lines.Length);

            foreach (var line in lines)
            {
                result.AppendLine(line.Substring(6, 66));
            }

            return result.ToString();
        }
    }
}
