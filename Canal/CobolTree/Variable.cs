
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

        public Variable(int level, string name, string code, Variable parent)
            : base(level.ToString(CultureInfo.InvariantCulture).PadLeft(2, '0') + "  " + name)
        {
            this.Level = level;
            this.Name = name;
            this.Code = code;
            this.Parent = parent;
            this.Variables = new List<Variable>();
        }

        public void FillNodesWithVariables()
        {
            this.Nodes.Clear();

            foreach (var variable in this.Variables)
            {
                this.Nodes.Add(variable);

                variable.FillNodesWithVariables();
            }
        }

        public override string ToString()
        {
            return string.Format("{0} {1}, Variables: {2}", this.Level, this.Name, this.Variables.Count);
        }
    }
}
