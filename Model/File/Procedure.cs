using Logging;
using Model.Enums;
using Model.Exceptions;
using Model.References;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;

namespace Model.File
{
    public class Procedure : CobolTreeNode
    {
        public override int StartIndex { get; }

        public override int EndIndex { get; }

        public override List<CobolTreeNode> GetNodes()
        {
            return new List<CobolTreeNode>();
        }

        #region References

        public List<PerformReference> PerformReferences { get; }

        public List<GoToReference> GoToReferences { get; }

        public List<PerformReference> IsReferencedBy { get; }

        public List<FileReference> CallReferences { get; }

        #endregion

        private int _linesOfCode;

        public ConcurrentDictionary<Variable, UsedAs> VariableUsages { get; }

        public Procedure(CobolFile cobolFile, string name, int beginIndex, int endIndex) : base(cobolFile, name)
        {
            StartIndex = beginIndex;
            EndIndex = endIndex;

            PerformReferences = new List<PerformReference>();
            GoToReferences = new List<GoToReference>();
            IsReferencedBy = new List<PerformReference>();
            CallReferences = new List<FileReference>();
            VariableUsages = new ConcurrentDictionary<Variable, UsedAs>();
        }

        public int GetLinesOfCode()
        {
            if (_linesOfCode == 0)
                _linesOfCode = GetCode().Split('\n', '\r').Count(text => !string.IsNullOrWhiteSpace(text)) - 1;

            return _linesOfCode;
        }

        public int GetLinesOfCodeRecursively(int depth = 20)
        {
            if (depth == 0)
            {
                Logger.Warning("Recursion limit of {0} reached while looking for Lines of Code recursively in procedure {1}.", 20, Name);
                throw new RecursionTooDeepException();
            }

            return GetLinesOfCode()
                + PerformReferences.Where(pref => pref.Procedure.Name != Name).Sum(pref => pref.Procedure.GetLinesOfCodeRecursively(depth - 1))
                   + GoToReferences.Where(gref => gref.Procedure.Name != Name).Sum(gref => gref.Procedure.GetLinesOfCodeRecursively(depth - 1));

        }

        public override string ToString()
        {
            return Name;
        }
    }
}
