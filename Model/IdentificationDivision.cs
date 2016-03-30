namespace Model
{
    public class IdentificationDivision : Division
    {
        public IdentificationDivision(string sourceCode, int indexIdentificationDivision)
                : base(sourceCode, "Identification Division", indexIdentificationDivision)
        {
            // AUTHOR, INSTALLATION, DATE-WRITTEN; DATE-COMPILED; SECURITY
        }
    }
}
