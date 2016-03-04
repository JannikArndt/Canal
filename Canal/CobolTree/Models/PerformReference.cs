
namespace Canal.CobolTree.Models
{

    public class PerformReference
    {
        public PerformReference(string referenceName)
        {
            this.ReferenceName = referenceName;
        }

        public string ReferenceName { get; set; }

        public Procedure Procedure { get; set; }

        public PerformType Type { get; set; }

        public override string ToString()
        {
            return string.Format("PERFORM {0}", this.ReferenceName);
        }
    }
}
