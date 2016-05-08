namespace Model
{
    public class IdentificationDivision : Division
    {
        // AUTHOR, INSTALLATION, DATE-WRITTEN; DATE-COMPILED; SECURITY

        protected override int StartIndex
        {
            get { return ParentCobolFile.DivisionsAndSection.Identification.GetValueOrDefault(-1); }
        }

        protected override int EndIndex
        {
            get { return ParentCobolFile.DivisionsAndSection.Environment.GetValueOrDefault(-1); }
        }

        public IdentificationDivision(CobolFile cobolFile) : base(cobolFile, "Identification Division")
        {
        }
    }
}
