namespace <!NAMESPACE!>
{
    /// <summary>
    /// Mapper from Byte Array to <!BUSINESSOBJECTNAME!> and back.
    /// </summary>
    public class <!BUSINESSOBJECTNAME!>Mapper : IMapper<<!BUSINESSOBJECTNAME!>>
    {
        /// <summary>
        /// Maps a byte array to a <!BUSINESSOBJECTNAME!>.
        /// </summary>
        /// <param name="bytes">A byte array</param>
        /// <returns>A <!BUSINESSOBJECTNAME!></returns>
        public <!BUSINESSOBJECTNAME!> Map(byte[] bytes)
        {
            return new <!BUSINESSOBJECTNAME!>
            {
<!MAPFROMCOBOL!>
            };
        }

        /// <summary>
        /// Maps a business object to a byte array.
        /// </summary>
        /// <param name="<!BONAMEPARAMETER!>">A <!BUSINESSOBJECTNAME!></param>
        /// <returns>A byte array</returns>
        public byte[] Map(<!BUSINESSOBJECTNAME!> <!BONAMEPARAMETER!>)
        {
            var bytes = new byte[<!BYTEARRAYSIZE!>];

<!MAPTOCOBOL!>

            return bytes;
        }
    }
}
