namespace Model.Pictures
{
    using System.Globalization;

    public class Pic9V9 : Pic9
    {
        private decimal? value;

        public int IntegersLength { get; set; }

        public int FractionsLength { get; set; }

        public override string Value
        {
            get { return this.value != null ? this.value.ToString() : null; }
            set { this.value = string.IsNullOrWhiteSpace(value) ? (decimal?)null : value.StartsWith("ZERO") ? 0 : decimal.Parse(value, CultureInfo.InvariantCulture); }
        }

        public Pic9V9(int integersLength, int fractionsLength, CompType comp = CompType.None)
            : base(integersLength + fractionsLength, comp)
        {
            this.IntegersLength = integersLength;
            this.FractionsLength = fractionsLength;
        }

        public override string ToString()
        {
            return string.Format("PIC 9{0}V9{1}", this.IntegersLength > 1 ? "(" + this.IntegersLength + ")" : string.Empty, this.FractionsLength > 1 ? "(" + this.FractionsLength + ")" : string.Empty);
        }
    }
}