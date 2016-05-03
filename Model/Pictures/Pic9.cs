namespace Model.Pictures
{
    public class Pic9 : IPic
    {
        private int? value;

        public bool Negative { get; set; }

        public int Length { get; set; }

        public CompType CompType { get; set; }

        public Pic9(int length, CompType comp = CompType.None, bool negative = false)
        {
            Length = length;
            CompType = comp;
            Negative = negative;
        }

        public virtual string Value
        {
            get { return value != null ? value.ToString() : null; }
            set { this.value = string.IsNullOrWhiteSpace(value) ? (int?)null : value.StartsWith("ZERO") ? 0 : int.Parse(value); }
        }

        public override string ToString()
        {
            return string.Format("PIC {0}9{1}", Negative ? "-" : "", Length > 1 ? "(" + Length + ")" : string.Empty);
        }
    }
}