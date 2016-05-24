using System;

namespace Model.Pictures
{
    public class PicS9V9 : Pic9V9
    {
        public PicS9V9(int integersLength, int fractionsLength, CompType comp = CompType.None)
            : base(integersLength, fractionsLength, comp)
        {
        }

        public new int ByteLength
        {
            get
            {
                switch (CompType)
                {
                    case CompType.None:
                        return IntegersLength + FractionsLength + 1;
                    case CompType.Comp3:
                        return (int)Math.Ceiling((double)(IntegersLength + FractionsLength + 1) / 2);
                    case CompType.Comp:
                    case CompType.Comp1:
                    case CompType.Comp2:
                    case CompType.Comp4:
                    default:
                        throw new NotImplementedException();
                }
            }
        }

        public override string ToString()
        {
            return string.Format("PIC S9{0}V9{1}", IntegersLength > 1 ? "(" + IntegersLength + ")" : string.Empty, FractionsLength > 1 ? "(" + FractionsLength + ")" : string.Empty);
        }
    }
}