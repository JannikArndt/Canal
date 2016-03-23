using FastColoredTextBoxNS.Enums;

namespace FastColoredTextBoxNS
{
    /// <summary>
    /// This Item does not check correspondence to current text fragment.
    /// SuggestItem is intended for dynamic menus.
    /// </summary>
    public class SuggestItem : AutocompleteItem
    {
        public SuggestItem(string text, int imageIndex):base(text, imageIndex)
        {   
        }

        public override CompareResult Compare(string fragmentText)
        {
            return CompareResult.Visible;
        }
    }
}