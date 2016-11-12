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

        private static IEnumerable<char> CharsToTitleCase(this string s)
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
                    yield return char.ToLower(c);
                }

            }
        }

        public static bool ContainsIgnoreCase(this string text, string query)
        {
            return text.IndexOf(query, StringComparison.OrdinalIgnoreCase) > -1;
        }

        public static bool None<TSource>(this IEnumerable<TSource> source)
        {
            return !source.Any();
        }

        public static bool None<TSource>(this IEnumerable<TSource> source, Func<TSource, bool> predicate)
        {
            return !source.Any(predicate);
        }

        /// <summary>
        /// Returns all distinct elements of the given source, where "distinctness"
        /// is determined via a projection and the default equality comparer for the projected type.
        /// https://github.com/morelinq/MoreLINQ/blob/master/MoreLinq/DistinctBy.cs
        /// </summary>
        /// <remarks>
        /// This operator uses deferred execution and streams the results, although
        /// a set of already-seen keys is retained. If a key is seen multiple times,
        /// only the first element with that key is returned.
        /// </remarks>
        /// <typeparam name="TSource">Type of the source sequence</typeparam>
        /// <typeparam name="TKey">Type of the projected element</typeparam>
        /// <param name="source">Source sequence</param>
        /// <param name="keySelector">Projection for determining "distinctness"</param>
        /// <returns>A sequence consisting of distinct elements from the source sequence,
        /// comparing them by the specified key projection.</returns>

        public static IEnumerable<TSource> DistinctBy<TSource, TKey>(this IEnumerable<TSource> source,
            Func<TSource, TKey> keySelector)
        {
            return source.DistinctBy(keySelector, null);
        }

        /// <summary>
        /// Returns all distinct elements of the given source, where "distinctness"
        /// is determined via a projection and the specified comparer for the projected type.
        /// https://github.com/morelinq/MoreLINQ/blob/master/MoreLinq/DistinctBy.cs
        /// </summary>
        /// <remarks>
        /// This operator uses deferred execution and streams the results, although
        /// a set of already-seen keys is retained. If a key is seen multiple times,
        /// only the first element with that key is returned.
        /// </remarks>
        /// <typeparam name="TSource">Type of the source sequence</typeparam>
        /// <typeparam name="TKey">Type of the projected element</typeparam>
        /// <param name="source">Source sequence</param>
        /// <param name="keySelector">Projection for determining "distinctness"</param>
        /// <param name="comparer">The equality comparer to use to determine whether or not keys are equal.
        /// If null, the default equality comparer for <c>TSource</c> is used.</param>
        /// <returns>A sequence consisting of distinct elements from the source sequence,
        /// comparing them by the specified key projection.</returns>
        public static IEnumerable<TSource> DistinctBy<TSource, TKey>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector, IEqualityComparer<TKey> comparer)
        {
            HashSet<TKey> knownKeys = new HashSet<TKey>(comparer);
            foreach (TSource element in source)
            {
                if (knownKeys.Add(keySelector(element)))
                {
                    yield return element;
                }
            }
        }
    }
}
