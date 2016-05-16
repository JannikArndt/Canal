using FastColoredTextBoxNS.Properties;
using System;
using System.Drawing;

namespace FastColoredTextBoxNS.Styles
{
    /// <summary>
    /// Marker style
    /// Draws background color for text
    /// </summary>
    public class SideMarkStyle : Style
    {
        public SideMarkStyle(MarkerStyle style)
        {
            IsExportable = true;
            _style = style;
        }

        private MarkerStyle _style;

        public override void Draw(Graphics gr, Point position, Range range)
        {

            Bitmap image;

            switch (_style)
            {
                case MarkerStyle.Perform:
                    image = Resources.performMarker;
                    break;
                case MarkerStyle.Goto:
                    image = Resources.gotoMarker;
                    break;
                case MarkerStyle.Call:
                    image = Resources.callMarker;
                    break;
                default:
                    throw new ArgumentOutOfRangeException();
            }

            // image.MakeTransparent(Color.Magenta);
            var symbol = new Rectangle(3, position.Y, image.Width, range.tb.CharHeight);

            gr.DrawImage(image, symbol);
            // gr.FillRectangle(Brushes.Aquamarine, symbol);


            AddVisualMarker(range.tb, new StyleVisualMarker(symbol, this) { Text = range.tb.Lines[range.Start.iLine] });
        }

        public override string GetCSS()
        {
            return "";
        }

        public enum MarkerStyle
        {
            Perform,
            Goto,
            Call
        }
    }
}