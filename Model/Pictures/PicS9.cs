using System;

namespace Model.Pictures
{
    public class PicS9 : Pic9
    {
        public PicS9(int length, CompType comp = CompType.None)
            : base(length, comp)
        {
        }

        public override string Value
        {
            get { return base.Value; }
            set { base.Value = value.Length > 1 ? value.Substring(1) : value; }
        }

        public new int ByteLength
        {
            get
            {
                switch (CompType)
                {
                    case CompType.None:
                        return Length + 1;
                    case CompType.Comp3:
                        return (int)Math.Ceiling((double)(Length + 1) / 2);
                    default:
                        throw new NotImplementedException();
                }
            }
        }

        public override string ToString()
        {
            return string.Format("PIC S9{0}", Length > 1 ? "(" + Length + ")" : string.Empty);
        }
    }
}