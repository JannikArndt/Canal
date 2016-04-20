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

        public override string ToString()
        {
            return string.Format("PIC S9{0}", this.Length > 1 ? "(" + this.Length + ")" : string.Empty);
        }
    }
}