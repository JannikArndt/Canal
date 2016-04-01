namespace CodeGenerator.Resources
{
    public interface IBusinessObject
    {
    }

    public class BusinessObjectExample : IBusinessObject
    {
        /// <summary>
        /// Contains Text
        /// [MY-TEXT]
        /// </summary>
        public string MyText { get; set; }
    }
}
