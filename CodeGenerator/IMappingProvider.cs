namespace CodeGenerator
{
    public interface IMappingProvider
    {
        /// <summary>
        /// Creates the code for a property definition, similar to <para />
        /// <code>&lt;summary&gt;My String [MY-TEXT]&lt;/summary&gt;<para />public String MyText { get; set; }</code>
        /// </summary>
        /// <returns>A formatted string</returns>
        string GetPropertyDefinition();

        /// <summary>
        /// <para>Creates the code for mapping a byte array to a property, similar to</para>
        /// <code> MyText = bytes.GetString(0, 10)</code>
        /// </summary>
        /// <returns>A formatted string</returns>
        string GetMappingFromCobol();

        /// <summary>
        /// <para>Creates the code for mapping a property to a byte array, similar to</para>
        /// <code>bytes.SetString(0, 10, myBusinessObject.MyText)</code>
        /// </summary>
        /// <param name="objectName">The name ob the object the property belongs to</param>
        /// <returns>A formatted string</returns>
        string GetMappingToCobol(string objectName);
    }
}