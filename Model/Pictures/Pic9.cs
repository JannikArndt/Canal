using System;

namespace Model.Pictures
{
    public class Pic9 : IPic
    {
        private long? value;

        public bool Negative { get; set; }

        public int Length { get; set; }

        public int ByteLength
        {
            get
            {
                switch (CompType)
                {
                    case CompType.None:
                        return Length;
                    case CompType.Comp:
                    case CompType.Comp3:
                        return (int)Math.Ceiling((double)Length / 2);
                    case CompType.Comp1:
                    case CompType.Comp2:
                    case CompType.Comp4:
                    default:
                        throw new NotImplementedException();
                }
            }
        }

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
            set { this.value = string.IsNullOrWhiteSpace(value) ? (long?)null : value.StartsWith("ZERO") ? 0 : long.Parse(value); }
        }

        public override string ToString()
        {
            return string.Format("PIC {0}9{1}", Negative ? "-" : "", Length > 1 ? "(" + Length + ")" : string.Empty);
        }
    }
}