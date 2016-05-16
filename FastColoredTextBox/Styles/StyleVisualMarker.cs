using System.Drawing;

namespace FastColoredTextBoxNS.Styles
{
    public class StyleVisualMarker : VisualMarker
    {
        public Style Style { get; private set; }

        public string Text { get; set; }

        public StyleVisualMarker(Rectangle rectangle, Style style)
            : base(rectangle)
        {
            Style = style;
        }
    }
}