namespace Model.Pictures
{
    public class PicX : IPic
    {
        private string value;

        public int Length { get; set; }

        public string Value
        {
            get { return this.value; }
            set { this.value = value.StartsWith("SPACE") ? new string(' ', this.Length) : value.Trim('\"', '"', '\\'); }
        }

        public CompType CompType { get; set; }

        public PicX(int length)
        {
            this.Length = length;
        }

        public override string ToString()
        {
            return string.Format("PIC X{0}", this.Length > 1 ? "(" + this.Length + ")" : string.Empty);
        }
    }
}