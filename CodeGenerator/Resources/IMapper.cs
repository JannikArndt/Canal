namespace CodeGenerator.Resources
{
    /// <summary>
    /// Mapper Interface
    /// </summary>
    public interface IMapper<TBusinessObjectType>
                where TBusinessObjectType : IBusinessObject, new()
    {
        /// <summary>
        /// Maps a byte array to a business object.
        /// </summary>
        /// <param name="bytes">A byte array</param>
        /// <returns>A business object</returns>
        TBusinessObjectType Map(byte[] bytes);

        /// <summary>
        /// Maps a business object to a byte array.
        /// </summary>
        /// <param name="businessObject">A business object</param>
        /// <returns>A byte array</returns>
        byte[] Map(TBusinessObjectType businessObject);
    }
}
