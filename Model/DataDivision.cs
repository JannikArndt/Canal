namespace Model
{
    public class DataDivision : Division
    {
        private WorkingStorageSection _workingStorageSection;
        private LinkageSection _linkageSection;

        public WorkingStorageSection WorkingStorageSection
        {
            get { return _workingStorageSection; }
            set
            {
                _workingStorageSection = value;
                Nodes.RemoveByKey(value.Text);
                Nodes.Add(value);
            }
        }

        public LinkageSection LinkageSection
        {
            get { return _linkageSection; }
            set
            {
                _linkageSection = value;
                Nodes.RemoveByKey(value.Text);
                Nodes.Add(value);
            }
        }

        protected override int StartIndex
        {
            get { return ParentCobolFile.DivisionsAndSection.Data.GetValueOrDefault(-1); }
        }

        protected override int EndIndex
        {
            get { return ParentCobolFile.DivisionsAndSection.Procedure.GetValueOrDefault(-1); }
        }

        public DataDivision(CobolFile cobolFile) : base(cobolFile, "Data Division")
        {
        }
    }
}
