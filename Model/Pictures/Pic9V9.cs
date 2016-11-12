using System;
using System.Globalization;

namespace Model.Pictures
{
    public class Pic9V9 : Pic9
    {
        private decimal? _value;

        public int IntegersLength { get; set; }

        public int FractionsLength { get; set; }

        public new int ByteLength
        {
            get
            {
                switch (CompType)
                {
                    case CompType.None:
                        return IntegersLength + FractionsLength;
                    case CompType.Comp3:
                        return (int)Math.Ceiling((double)(IntegersLength + FractionsLength) / 2);
                    default:
                        throw new NotImplementedException();
                }
            }
        }

        public override string Value
        {
            get { return _value != null ? _value.ToString() : null; }
            set { _value = string.IsNullOrWhiteSpace(value) ? (decimal?)null : value.StartsWith("ZERO") ? 0 : decimal.Parse(value, CultureInfo.InvariantCulture); }
        }

        public Pic9V9(int integersLength, int fractionsLength, CompType comp = CompType.None)
            : base(integersLength + fractionsLength, comp)
        {
            IntegersLength = integersLength;
            FractionsLength = fractionsLength;
        }

        public override string ToString()
        {
            return string.Format("PIC 9{0}V9{1}", IntegersLength > 1 ? "(" + IntegersLength + ")" : string.Empty, FractionsLength > 1 ? "(" + FractionsLength + ")" : string.Empty);
        }
    }
}