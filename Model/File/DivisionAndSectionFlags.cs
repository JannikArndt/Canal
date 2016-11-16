namespace Model.File
{
    /// <summary>
    /// Contains the character index of all divisions and sections
    /// </summary>
    public class DivisionAndSectionFlags
    {
        /// <summary>
        /// Position of the first character of the word IDENTIFICATION DIVISION
        /// </summary>
        public int? Identification { get; set; }

        /// <summary>
        /// Position of the first character of the word  ENVIRONMENT DIVISION
        /// </summary>
        public int? Environment { get; set; }

        /// <summary>
        /// Position of the first character of the word  DATA DIVISION
        /// </summary>
        public int? Data { get; set; }

        /// <summary>
        /// Position of the first character of the word  PROCEDURE DIVISION
        /// </summary>
        public int? Procedure { get; set; }

        /// <summary>
        /// Position of the first character of the word  WORKING-STORAGE SECTION
        /// </summary>
        public int? WorkingStorage { get; set; }

        /// <summary>
        /// Position of the first character of the word LINKAGE SECTION
        /// </summary>
        public int? Linkage { get; set; }

        /// <summary>
        /// true if all divisions have been found
        /// </summary>
        /// <returns></returns>
        public bool AllDivisionsFound()
        {
            return Identification.HasValue && Environment.HasValue && Data.HasValue && Procedure.HasValue;
        }

        /// <summary>
        /// true if als divisions except for the procedure devision have been found and are in order
        /// </summary>
        /// <returns></returns>
        public bool ProcedureMissing()
        {
            return Identification.HasValue && Environment.HasValue && Environment.Value > Identification.Value
                   && Data.HasValue && Data.Value > Environment.Value && (!Procedure.HasValue || Procedure.Value == 0);
        }
    }
}