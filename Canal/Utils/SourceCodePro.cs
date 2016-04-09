using System.Drawing;
using System.Drawing.Text;

namespace Canal.Utils
{
    public static class SourceCodePro
    {
        // ReSharper disable once PrivateFieldCanBeConvertedToLocalVariable // Don't listen to ReSharper here, it has to stay static!
        private static readonly PrivateFontCollection Pfc;

        public static readonly Font Regular;
        public static readonly Font Bold;
        public static readonly Font Italic;
        // public static readonly Font Black;
        // public static readonly Font ExtraLight;
        // public static readonly Font Light;
        // public static readonly Font Medium;
        // public static readonly Font SemiBold;


        static SourceCodePro()
        {
            Pfc = new PrivateFontCollection();
            Pfc.AddFontFile("Resources/SourceCodePro-Regular.ttf");
            Pfc.AddFontFile("Resources/SourceCodePro-Bold.ttf");
            Pfc.AddFontFile("Resources/SourceCodePro-BoldIt.ttf");
            Pfc.AddFontFile("Resources/SourceCodePro-It.ttf");

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

            Regular = new Font(Pfc.Families[0], 10f, FontStyle.Regular);
            Bold = new Font(Pfc.Families[0], 10f, FontStyle.Bold);
            Italic = new Font(Pfc.Families[0], 10f, FontStyle.Italic);
            // Light = new Font(Pfc.Families[1], 10f, FontStyle.Regular | FontStyle.Italic);
            // Medium = new Font(Pfc.Families[2], 10f, FontStyle.Bold | FontStyle.Italic);
            // Black = new Font(Pfc.Families[3], 10f, FontStyle.Bold | FontStyle.Italic);
            // ExtraLight = new Font(Pfc.Families[4], 10f, FontStyle.Regular | FontStyle.Italic);
            // SemiBold = new Font(Pfc.Families[5], 10f, FontStyle.Bold | FontStyle.Italic);
        }
    }
}
