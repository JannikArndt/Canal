namespace Model.Pictures
{
    public class PicGroup : IPic
    {
        private const int length = 0;

        private const string value = "";

        private const CompType compType = CompType.None;

        public int Length
        {
            get { return length; }
            set { }
        }

        public string Value
        {
            get { return value; }
            set { }
        }

        public CompType CompType
        {
            get { return compType; }
            set { }
        }

        public int ByteLength { get { return 0; } }

        public override string ToString()
        {
            return string.Empty;
        }
    }
}