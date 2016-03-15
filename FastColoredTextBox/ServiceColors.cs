using System;
using System.Drawing;

namespace FastColoredTextBoxNS
{
    [Serializable]
    public class ServiceColors
    {
        public Color CollapseMarkerForeColor { get; set; }
        public Color CollapseMarkerBackColor { get; set; }
        public Color CollapseMarkerBorderColor { get; set; }
        public Color ExpandMarkerForeColor { get; set; }
        public Color ExpandMarkerBackColor { get; set; }
        public Color ExpandMarkerBorderColor { get; set; }

        public ServiceColors()
        {
            CollapseMarkerForeColor = Color.Silver;
            CollapseMarkerBackColor = Color.White;
            CollapseMarkerBorderColor = Color.Silver;
            ExpandMarkerForeColor = Color.Red;
            ExpandMarkerBackColor = Color.White;
            ExpandMarkerBorderColor = Color.Silver;
        }
    }
}