using FastColoredTextBoxNS.Enums;

namespace Level88ToEnum
{
    using System;
    using System.Collections.Generic;
    using System.Windows.Forms;

    using FastColoredTextBoxNS;

    using global::Level88ToEnum.Properties;


    public partial class Level88ToEnum : Form
    {
        public Level88ToEnum()
        {
            this.InitializeComponent();

            // Inputs
            this.cobolLevel88Input.TextChanged += (sender, args) => this.CreateOutput();

            this.csharpNamesInput.TextChanged += (sender, args) => this.CreateOutput();

            this.commentsInput.TextChanged += (sender, args) => this.CreateOutput();

            // Outputs
            this.csharpEnumOutput.Language = Language.CSharp;
            this.csharpEnumOutput.HighlightingRangeType = HighlightingRangeType.VisibleRange;
            this.csharpEnumOutput.SyntaxHighlighter.HighlightSyntax(Language.CSharp, this.csharpEnumOutput.Range);

            this.csharpEnumMapperOutput.Language = Language.CSharp;
            this.csharpEnumMapperOutput.HighlightingRangeType = HighlightingRangeType.VisibleRange;
            this.csharpEnumMapperOutput.SyntaxHighlighter.HighlightSyntax(Language.CSharp, this.csharpEnumMapperOutput.Range);

            this.namespaceEnumInput.Text = Settings.Default.NamespaceEnum;
            this.namespaceMapperInput.Text = Settings.Default.NamespaceMapper;
        }

        private void CreateOutput()
        {
            var inputCobol = this.cobolLevel88Input.Text.Split(new[] { Environment.NewLine }, StringSplitOptions.RemoveEmptyEntries);
            var inputCsharp = this.csharpNamesInput.Text.Split(new[] { Environment.NewLine }, StringSplitOptions.RemoveEmptyEntries);
            var inputComments = this.commentsInput.Text.Split(new[] { Environment.NewLine }, StringSplitOptions.RemoveEmptyEntries);

            var enumName = string.IsNullOrWhiteSpace(this.settingsName.Text) ? "NewEnum" : this.settingsName.Text;
            var enumNamespace = string.IsNullOrWhiteSpace(this.namespaceEnumInput.Text) ? "Solution.Project.Enums" : this.namespaceEnumInput.Text;
            var mapperNamespace = string.IsNullOrWhiteSpace(this.namespaceMapperInput.Text) ? "Solution.Project.Mappers" : this.namespaceMapperInput.Text;

            this.csharpEnumOutput.Text = this.CreateEnum(enumName, enumNamespace, inputCobol, inputCsharp, inputComments);
            this.csharpEnumMapperOutput.Text = this.CreateMapper(enumName, mapperNamespace, enumNamespace, inputCobol, inputCsharp, inputComments);
        }

        private string CreateEnum(string name, string enumNamespace, IList<string> inputCobol, IList<string> inputCsharp, IList<string> inputComments)
        {
            string namespaceText = "using System;\n\nnamespace " + enumNamespace + "\n{\n";
            string classComment = "    /// <summary>\n    /// Enum\n    /// </summary>\n";
            string header = "    public enum " + name + "\n" + "    {\n";
            var enumList = new List<string>();

            for (var index = 0; index < inputCsharp.Count; index++)
            {
                string commentText = inputComments.Count > index ? inputComments[index] : inputCsharp[index];
                string cobolName = inputCobol.Count > index ? "        /// entspricht dem Wert " + inputCobol[index] + "\n" : string.Empty;

                enumList.Add("        /// <summary>\n"
                    + "        /// " + commentText + "\n" + cobolName
                    + "        /// </summary>\n        " + inputCsharp[index]);
            }

            enumList.Add("        /// <summary>\n" + "        /// kein Wert gesetzt\n" + "        /// </summary>\n        Undefined");

            var ending = "\n    }\n}";

            return namespaceText + classComment + header + string.Join(",\n\n", enumList) + ending;
        }

        private string CreateMapper(string enumName, string mapperNamespace, string enumNamespace, IList<string> inputCobol, IList<string> inputCsharp, IList<string> inputComments)
        {
            string namespaceText = "namespace " + mapperNamespace + "\n{\n";
            string usingText = mapperNamespace != enumNamespace ? "    using " + enumNamespace + ";\n\n" : string.Empty;
            string classComment = "    /// <summary>\n    /// Mapper für " + enumName + "\n    /// </summary>\n";
            string header = "    public static class " + enumName + "Mapper\n" + "    {\n";

            // Direction: COBOL => C#
            string method1Comment = "        /// <summary>\n        /// Mappt einen COBOL-Wert auf das Enum " + enumName + ".\n        /// </summary>\n"
                + "        /// <param name=\"text\">Ein String mit einem Zeichen</param>\n"
                + "        /// <returns>Das gemappte Enum " + enumName + "</returns>\n";
            string method1 = "        public static " + enumName + " Map(string text)\n        {\n";
            string switch1 = "            switch(text[0])\n            {\n";

            var caseList1 = new List<string>();

            for (var index = 0; index < inputCsharp.Count; index++)
            {
                string cobolName = inputCobol.Count > index ? inputCobol[index] : index.ToString();
                caseList1.Add("                case '" + cobolName + "': return " + enumName + "." + inputCsharp[index] + ";");
            }

            caseList1.Add("                default: return " + enumName + ".Undefined;");

            var ending1 = "\n            }\n        }\n\n";

            // Direction: C# => COBOL
            string parameterName = char.ToLowerInvariant(enumName[0]) + enumName.Substring(1);
            string method2Comment = "        /// <summary>\n        /// Mappt einen Wert des Enum " + enumName + " auf einen COBOL-Wert.\n        /// </summary>\n"
                + "        /// <param name=\"" + parameterName + "\">Ein Enum</param>\n"
                + "        /// <returns>Der entsprechende COBOL-Wert</returns>\n";
            string method2 = "        public static string Map(" + enumName + " " + parameterName + ")\n        {\n";
            string switch2 = "            switch(" + parameterName + ")\n            {\n";

            var caseList2 = new List<string>();

            for (var index = 0; index < inputCsharp.Count; index++)
            {
                string cobolName = inputCobol.Count > index ? inputCobol[index] : index.ToString();
                caseList2.Add("                case " + enumName + "." + inputCsharp[index] + ": return \"" + cobolName + "\";");
            }

            caseList2.Add("                default: return \" \";");

            var ending2 = "\n            }\n        }\n    }\n}";

            return namespaceText + usingText + classComment + header
                + method1Comment + method1 + switch1 + string.Join("\n", caseList1) + ending1
                + method2Comment + method2 + switch2 + string.Join("\n", caseList2) + ending2;
        }

        private void settingsChanged(object sender, EventArgs e)
        {
            Settings.Default.NamespaceEnum = this.namespaceEnumInput.Text;
            Settings.Default.NamespaceMapper = this.namespaceMapperInput.Text;
            Settings.Default.Save();
            this.CreateOutput();
        }
    }
}
