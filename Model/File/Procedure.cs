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
        public sealed override int StartIndex { get; protected set; }

        public sealed override int EndIndex { get; protected set; }

        public override List<CobolTreeNode> GetNodes()
        {
            return new List<CobolTreeNode>();
        }

        #region References

        public List<PerformReference> PerformReferences { get; private set; }

        public List<GoToReference> GoToReferences { get; private set; }

        public List<PerformReference> IsReferencedBy { get; private set; }

        public List<FileReference> CallReferences { get; private set; }

        #endregion

        private int _linesOfCode;

        public ConcurrentDictionary<Variable, UsedAs> VariableUsages { get; private set; }

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
                + PerformReferences.Where(pref => pref.Procedure != null && pref.Procedure.Name != Name)
                                   .Sum(pref => pref.Procedure.GetLinesOfCodeRecursively(depth - 1))
                   + GoToReferences.Where(gref => gref.Procedure != null && gref.Procedure.Name != Name)
                                   .Sum(gref => gref.Procedure.GetLinesOfCodeRecursively(depth - 1));

        }

        public override string ToString()
        {
            return Name;
        }
    }
}
