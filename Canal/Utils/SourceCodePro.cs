using System.Drawing;
using System.Drawing.Text;

namespace Canal.Utils
{
    public static class SourceCodePro
    {
        private static readonly PrivateFontCollection pfc;

        public static readonly Font Regular;
        public static readonly Font Black;
        public static readonly Font ExtraLight;
        public static readonly Font Light;
        public static readonly Font Medium;
        public static readonly Font SemiBold;


        static SourceCodePro()
        {
            pfc = new PrivateFontCollection();
            pfc.AddFontFile("Resources/SourceCodePro-Regular.ttf");
            pfc.AddFontFile("Resources/SourceCodePro-Bold.ttf");
            pfc.AddFontFile("Resources/SourceCodePro-BoldIt.ttf");
            pfc.AddFontFile("Resources/SourceCodePro-It.ttf");

            pfc.AddFontFile("Resources/SourceCodePro-Light.ttf");
            pfc.AddFontFile("Resources/SourceCodePro-LightIt.ttf");

            pfc.AddFontFile("Resources/SourceCodePro-Medium.ttf");
            pfc.AddFontFile("Resources/SourceCodePro-MediumIt.ttf");

            pfc.AddFontFile("Resources/SourceCodePro-Black.ttf");
            pfc.AddFontFile("Resources/SourceCodePro-BlackIt.ttf");

            pfc.AddFontFile("Resources/SourceCodePro-ExtraLight.ttf");
            pfc.AddFontFile("Resources/SourceCodePro-ExtraLightIt.ttf");

            pfc.AddFontFile("Resources/SourceCodePro-SemiBold.ttf");
            pfc.AddFontFile("Resources/SourceCodePro-SemiBoldIt.ttf");

            Regular = new Font(pfc.Families[0], 10f, FontStyle.Regular | FontStyle.Bold | FontStyle.Italic);
            Light = new Font(pfc.Families[1], 10f, FontStyle.Regular | FontStyle.Italic);
            Medium = new Font(pfc.Families[2], 10f, FontStyle.Bold | FontStyle.Italic);
            Black = new Font(pfc.Families[3], 10f, FontStyle.Bold | FontStyle.Italic);
            ExtraLight = new Font(pfc.Families[4], 10f, FontStyle.Regular | FontStyle.Italic);
            SemiBold = new Font(pfc.Families[5], 10f, FontStyle.Bold | FontStyle.Italic);
        }
    }
}
