namespace Model.Pictures
{
    public interface IPic
    {
        int Length { get; set; }

        string Value { get; set; }

        CompType CompType { get; set; }

        int ByteLength { get; }
    }
}