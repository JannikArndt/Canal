namespace Model.Pictures
{
    public class PicGroup : IPic
    {
        public int Length
        {
            get { return 0; }
            set { }
        }

        public string Value
        {
            get { return ""; }
            set { }
        }

        public CompType CompType
        {
            get { return CompType.None; }
            set { }
        }

        public int ByteLength { get { return 0; } }

        public override string ToString()
        {
            return string.Empty;
        }
    }
}