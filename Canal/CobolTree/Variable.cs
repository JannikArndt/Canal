
namespace Canal.CobolTree
{
    using System.Collections.Generic;
    using System.Globalization;
    using System.Windows.Forms;

    public class Variable : TreeNode
    {
        public int Level { get; set; }

        public string Name { get; set; }

        public string Code { get; set; }

        public List<Variable> Variables { get; set; }

        public Variable Parent { get; set; }

        public Variable Root
        {
            get
            {
                if (Parent == null) return null;

                var result = Parent;
                while ((result.Level != 1 || result.Level != 77) && result.Parent != null)
                    result = result.Parent;
                return result;
            }
        }

        public Variable(int level, string name, string code, Variable parent)
            : base(level.ToString(CultureInfo.InvariantCulture).PadLeft(2, '0') + "  " + name)
        {
            Level = level;
            Name = name;
            Code = code;
            Parent = parent;
            Variables = new List<Variable>();
        }

        public void FillNodesWithVariables()
        {
            Nodes.Clear();

            foreach (var variable in Variables)
            {
                Nodes.Add(variable);

                variable.FillNodesWithVariables();
            }
        }

        public override string ToString()
        {
            return string.Format("{0} {1}, Variables: {2}", Level, Name, Variables.Count);
        }
    }
}
