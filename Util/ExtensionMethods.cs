using Model.Enums;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace Util
{
    public static class ExtensionMethods
    {
        public static string ToText(this TreeView treeView)
        {
            var result = new StringBuilder();

            foreach (TreeNode node in treeView.Nodes)
                result.AppendLine(node.ToText());

            return result.ToString();
        }

        public static string ToText(this TreeNode treeNode, string indent = "")
        {
            var result = new StringBuilder();

            result.AppendLine(indent + treeNode.Text);

            foreach (TreeNode node in treeNode.Nodes)
                result.Append(node.ToText(indent + "    "));

            return result.ToString();
        }

        public static string ToShortString(this UsedAs usedAs)
        {
            switch (usedAs)
            {
                case UsedAs.Unknown:
                    return "";
                case UsedAs.Input:
                    return "(in)";
                case UsedAs.Output:
                    return "(out)";
                case UsedAs.Both:
                    return "(in/out)";
                default:
                    throw new ArgumentOutOfRangeException("usedAs", usedAs, null);
            }
        }

        public static UsedAs MergeUsages(this UsedAs usedAs, Literal literal)
        {
            if (usedAs == UsedAs.Both)
                return UsedAs.Both;

            if (usedAs == UsedAs.Input)
                if (literal.UsedAs == UsedAs.Output)
                    return UsedAs.Both;
                else
                    return UsedAs.Input;

            if (usedAs == UsedAs.Output)
                if (literal.UsedAs == UsedAs.Input)
                    return UsedAs.Both;
                else
                    return UsedAs.Output;

            return UsedAs.Unknown;
        }

        public static string ToPropertyName(this string s)
        {
            return new string(s.CharsToTitleCase().ToArray());
        }

        public static IEnumerable<char> CharsToTitleCase(this string s)
        {
            bool newWord = true;
            foreach (char c in s)
            {
                if (c == ' ' || c == '-' || char.IsDigit(c))
                {
                    newWord = true;
                }
                else if (newWord)
                {
                    yield return char.ToUpper(c);
                    newWord = false;
                }
                else
                {
                    yield return Char.ToLower(c);
                }

            }
        }
    }
}
