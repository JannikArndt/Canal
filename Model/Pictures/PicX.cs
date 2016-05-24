namespace Model.Pictures
{
    public class PicX : IPic
    {
        private string value;

        public int Length { get; set; }

        public string Value
        {
            get { return value; }
            set { this.value = value.StartsWith("SPACE") ? new string(' ', Length) : value.Trim('\"', '"', '\\'); }
        }

        public CompType CompType { get; set; }

        public int ByteLength { get { return Length; } }

        public PicX(int length)
        {
            Length = length;
        }

        public override string ToString()
        {
            return string.Format("PIC X{0}", Length > 1 ? "(" + Length + ")" : string.Empty);
        }
    }
}