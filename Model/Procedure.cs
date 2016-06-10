using Model.Enums;
using Model.References;
using System.Collections.Concurrent;
using System.Collections.Generic;

namespace Model
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

        public override string ToString()
        {
            return Name;
        }
    }
}
