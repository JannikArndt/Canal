namespace Model.Pictures
{
    public class Pic9 : IPic
    {
        private int? value;

        public int Length { get; set; }

        public CompType CompType { get; set; }

        public Pic9(int length, CompType comp = CompType.None)
        {
            this.Length = length;
            this.CompType = comp;
        }

        public virtual string Value
        {
            get { return this.value != null ? this.value.ToString() : null; }
            set { this.value = string.IsNullOrWhiteSpace(value) ? (int?)null : value.StartsWith("ZERO") ? 0 : int.Parse(value); }
        }

        public override string ToString()
        {
            return string.Format("PIC 9{0}", this.Length > 1 ? "(" + this.Length + ")" : string.Empty);
        }
    }
}