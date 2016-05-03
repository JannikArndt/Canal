namespace Model.Pictures
{
    public class PicS9V9 : Pic9V9
    {
        public PicS9V9(int integersLength, int fractionsLength, CompType comp = CompType.None)
            : base(integersLength, fractionsLength, comp)
        {
        }

        public override string ToString()
        {
            return string.Format("PIC S9{0}V9{1}", IntegersLength > 1 ? "(" + IntegersLength + ")" : string.Empty, FractionsLength > 1 ? "(" + FractionsLength + ")" : string.Empty);
        }
    }
}