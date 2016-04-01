namespace CodeGenerator.Resources
{
    public interface IMapper
    {
        IBusinessObject Map(byte[] bytes);
    }

    public class MapperExample : IMapper
    {
        public IBusinessObject Map(byte[] bytes)
        {
            return new BusinessObjectExample
            {
                MyText = bytes.GetString(0, 10)
            };
        }
    }
}
