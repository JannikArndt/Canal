namespace Model.Pictures
{
    public class PicBinary : IPic
    {
        public int Length
        {
            get { return 1; }
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

        public int ByteLength { get { return 1; } }

        public override string ToString()
        {
            return "BINARY";
        }
    }
}
