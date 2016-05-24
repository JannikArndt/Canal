namespace Model.Pictures
{
    public class Pic88 : IPic
    {
        public int Length { get; set; }
        public string Value { get; set; }
        public CompType CompType { get; set; }
        public int ByteLength { get { return 0; } }

        public override string ToString()
        {
            return string.Format("VALUE {0}", Value);
        }
    }
}