using System.Drawing;
using System.Drawing.Text;

namespace Canal.Utils
{
    public class SourceCodePro
    {
        public static readonly SourceCodePro Instance = new SourceCodePro();

        public Font Regular()
        {
            return new Font(_pfc.Families[0], 10f, FontStyle.Regular);
        }

        public Font Bold()
        {
            return new Font(_pfc.Families[0], 10f, FontStyle.Bold);
        }

        public Font Italic()
        {
            return new Font(_pfc.Families[0], 10f, FontStyle.Italic);
        }

        private readonly PrivateFontCollection _pfc;

        private SourceCodePro()
        {
            _pfc = new PrivateFontCollection();
            _pfc.AddFontFile("Resources/SourceCodePro-Regular.ttf");
            _pfc.AddFontFile("Resources/SourceCodePro-Bold.ttf");
            _pfc.AddFontFile("Resources/SourceCodePro-BoldIt.ttf");
            _pfc.AddFontFile("Resources/SourceCodePro-It.ttf");

            // Pfc.AddFontFile("Resources/SourceCodePro-Light.ttf");
            // Pfc.AddFontFile("Resources/SourceCodePro-LightIt.ttf");

            // Pfc.AddFontFile("Resources/SourceCodePro-Medium.ttf");
            // Pfc.AddFontFile("Resources/SourceCodePro-MediumIt.ttf");

            // Pfc.AddFontFile("Resources/SourceCodePro-Black.ttf");
            // Pfc.AddFontFile("Resources/SourceCodePro-BlackIt.ttf");

            // Pfc.AddFontFile("Resources/SourceCodePro-ExtraLight.ttf");
            // Pfc.AddFontFile("Resources/SourceCodePro-ExtraLightIt.ttf");

            // Pfc.AddFontFile("Resources/SourceCodePro-SemiBold.ttf");
            // Pfc.AddFontFile("Resources/SourceCodePro-SemiBoldIt.ttf");

            // Regular = new Font(Pfc.Families[0], 10f, FontStyle.Regular);
            // Bold = new Font(Pfc.Families[0], 10f, FontStyle.Bold);
            // Italic = new Font(Pfc.Families[0], 10f, FontStyle.Italic);
            // Light = new Font(Pfc.Families[1], 10f, FontStyle.Regular | FontStyle.Italic);
            // Medium = new Font(Pfc.Families[2], 10f, FontStyle.Bold | FontStyle.Italic);
            // Black = new Font(Pfc.Families[3], 10f, FontStyle.Bold | FontStyle.Italic);
            // ExtraLight = new Font(Pfc.Families[4], 10f, FontStyle.Regular | FontStyle.Italic);
            // SemiBold = new Font(Pfc.Families[5], 10f, FontStyle.Bold | FontStyle.Italic);
        }
    }
}
