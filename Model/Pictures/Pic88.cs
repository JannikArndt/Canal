namespace Model.Pictures
{
    public class Pic88 : IPic
    {
        public int Length { get; set; }
        public string Value { get; set; }
        public CompType CompType { get; set; }

        public override string ToString()
        {
            return string.Format("VALUE {0}", this.Value);
        }
    }
}