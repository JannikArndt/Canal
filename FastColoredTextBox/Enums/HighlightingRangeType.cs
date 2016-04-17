namespace FastColoredTextBoxNS.Enums
{
    /// <summary>
    /// Type of highlighting
    /// </summary>
    public enum HighlightingRangeType
    {
        /// <summary>
        /// Highlight only changed range of text. Highest performance.
        /// </summary>
        ChangedRange,

        /// <summary>
        /// Highlight visible range of text. Middle performance.
        /// </summary>
        VisibleRange,

        /// <summary>
        /// Highlight all (visible and invisible) text. Lowest performance.
        /// </summary>
        AllTextRange,

        /// <summary>
        /// Highlights only first 100 lines, rest must be triggered manually.
        /// </summary>
        First100Lines
    }
}