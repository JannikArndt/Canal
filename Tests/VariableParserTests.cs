using Microsoft.VisualStudio.TestTools.UnitTesting;
using Model.Pictures;
using System.Collections.Generic;

namespace Tests
{
    [TestClass]
    public class VariableParserTests
    {
        [TestMethod]
        public void VariableParserTest_Group()
        {
            const string input = " 01 FOO-1-BAR.       ";

            var actual = Canal.Utils.VariablesUtil.AnalyzeVariables(input);

            Assert.AreEqual(1, actual.Count, input);
            Assert.AreEqual(typeof(PicGroup), actual[0].Picture.GetType());
        }

        [TestMethod]
        public void VariableParserTest_Simple()
        {
            var strings = new List<string>
            {
                " 01 FOO-1-BAR PICTURE X.                   ",
                " 01 FOO-1-BAR PICTURE IS X.                ",
                " 01 FOO-1-BAR PIC X.                       ",
                " 01 FOO-1-BAR PIC IS X.                    ",
                " 01 FOO-1-BAR PIC X(04).                   ",
                " 01 FOO-1-BAR PIC 9.                       ",
                " 01 FOO-1-BAR PIC 999.                     ",
                " 01 FOO-1-BAR PIC 999 COMP.                ",
                " 01 FOO-1-BAR PIC 999 COMP-3.              ",
                " 01 FOO-1-BAR PIC 999 COMPUTATIONAL-4.     ",
                " 01 FOO-1-BAR PIC 9(5).                    ",
                " 01 FOO-1-BAR PIC S9.                      ",
                " 01 FOO-1-BAR PIC S9(51).                  ",
                " 01 FOO-1-BAR PIC 9V9.                     ",
                " 01 FOO-1-BAR PIC 9(3)V99.                 ",
                " 01 FOO-1-BAR PIC S9V99.                   ",
                " 01 FOO-1-BAR PIC V99.                     "
            };

            foreach (var s in strings)
            {
                var actual = Canal.Utils.VariablesUtil.AnalyzeVariables(s);

                Assert.AreEqual(1, actual.Count, s);
            }
        }

        [TestMethod]
        public void VariableParserTest_WithValue_String()
        {
            const string input = " 01 FOO-1-BAR PIC X VALUE \"asdjh\".       ";

            var actual = Canal.Utils.VariablesUtil.AnalyzeVariables(input);

            Assert.AreEqual(1, actual.Count, input);
            Assert.AreEqual("FOO-1-BAR", actual[0].VariableName);
        }

        [TestMethod]
        public void VariableParserTest_WithValue_Spaces()
        {
            const string input = " 01 FOO-1-BAR PIC X(04) VALUE SPACES.     ";

            var actual = Canal.Utils.VariablesUtil.AnalyzeVariables(input);

            Assert.AreEqual(1, actual.Count, input);
            Assert.AreEqual("FOO-1-BAR", actual[0].VariableName);
        }

        [TestMethod]
        public void VariableParserTest_WithValue_Int()
        {
            const string input = " 01 FOO-1-BAR PIC 9 VALUE 3.";

            var actual = Canal.Utils.VariablesUtil.AnalyzeVariables(input);

            Assert.AreEqual(1, actual.Count, input);
            Assert.AreEqual("FOO-1-BAR", actual[0].VariableName);
        }

        [TestMethod]
        public void VariableParserTest_WithValue_Decimal()
        {
            const string input = " 01 FOO-1-BAR PIC S9V99 VALUE 2.43.";

            var actual = Canal.Utils.VariablesUtil.AnalyzeVariables(input);

            Assert.AreEqual(1, actual.Count, input);
            Assert.AreEqual("FOO-1-BAR", actual[0].VariableName);
        }


        [TestMethod]
        public void VariableParserTest_WithValue_More()
        {
            var strings = new List<string>
            {
                " 01 FOO-1-BAR PIC 9 VALUE HIGH-VALUES.     ",
                " 01 FOO-1-BAR PIC 9 VALUE LOW-VALUES.      ",
                " 01 FOO-1-BAR PIC 9 VALUE ZERO.            ",
                " 01 FOO-1-BAR PIC 999 VALUE 432.           ",
                " 01 FOO-1-BAR PIC 9(5) VALUE 12397.        ",
                " 01 FOO-1-BAR PIC S9 VALUE 4.              ",
                " 01 FOO-1-BAR PIC S9(51) VALUE 99283468723.",
                " 01 FOO-1-BAR PIC 9V9 VALUE 9.3.           ",
                " 01 FOO-1-BAR PIC 9(3)V99 VALUE 123.54.    ",
                " 01 FOO-1-BAR PIC V99 VALUE .43.           "
            };

            foreach (var s in strings)
            {
                var actual = Canal.Utils.VariablesUtil.AnalyzeVariables(s);

                Assert.AreEqual(1, actual.Count, s);
            }
        }

        [TestMethod]
        public void VariableParserTest_WithOccurs()
        {
            const string input = " 01 FOO-1-BAR PIC 999 OCCURS 20.";

            var actual = Canal.Utils.VariablesUtil.AnalyzeVariables(input);

            Assert.AreEqual(1, actual.Count, input);
            Assert.AreEqual("FOO-1-BAR", actual[0].VariableName);
        }

        [TestMethod]
        public void VariableParserTest_WithRedefines()
        {
            const string input = " 03 FOO-2-BAR REDEFINES FOO-1-BAR PIC 9.";

            var actual = Canal.Utils.VariablesUtil.AnalyzeVariables(input);

            Assert.AreEqual(1, actual.Count, input);
            Assert.AreEqual("FOO-1-BAR", actual[0].Redefines);
            Assert.AreEqual("FOO-2-BAR", actual[0].VariableName);
        }

        [TestMethod]
        public void VariableParserTest_Level88_Int()
        {
            const string input = " 88 FOO-1-BAR VALUE 1.";

            var actual = Canal.Utils.VariablesUtil.AnalyzeVariables(input);

            Assert.AreEqual(1, actual.Count, input);
            Assert.AreEqual(typeof(Pic88), actual[0].Picture.GetType());
            Assert.AreEqual("FOO-1-BAR", actual[0].VariableName);
            Assert.AreEqual("1", actual[0].Picture.Value);
        }

        [TestMethod]
        public void VariableParserTest_Level88_String()
        {
            const string input = " 88 FOO-1-BAR VALUE \"foo\".                     ";

            var actual = Canal.Utils.VariablesUtil.AnalyzeVariables(input);

            Assert.AreEqual(1, actual.Count, input);
            Assert.AreEqual(typeof(Pic88), actual[0].Picture.GetType());
            Assert.AreEqual("FOO-1-BAR", actual[0].VariableName);
            Assert.AreEqual("\"foo\"", actual[0].Picture.Value);
        }

        [TestMethod]
        public void VariableParserTest_Level88_EmptyString()
        {
            const string input = " 88 FOO-1-BAR VALUE \" \".";

            var actual = Canal.Utils.VariablesUtil.AnalyzeVariables(input);

            Assert.AreEqual(1, actual.Count, input);
            Assert.AreEqual(typeof(Pic88), actual[0].Picture.GetType());
            Assert.AreEqual("FOO-1-BAR", actual[0].VariableName);
            Assert.AreEqual("\" \"", actual[0].Picture.Value);
        }

        [TestMethod]
        public void VariableParserTest_Level88_THRU()
        {
            const string input = " 88  FOO-1-BAR VALUE \" \"   THRU \"2\".";

            var actual = Canal.Utils.VariablesUtil.AnalyzeVariables(input);

            Assert.AreEqual(1, actual.Count, input);
            Assert.AreEqual(typeof(Pic88), actual[0].Picture.GetType());
            Assert.AreEqual("FOO-1-BAR", actual[0].VariableName);
            Assert.AreEqual("\" \"   THRU \"2\"", actual[0].Picture.Value);
        }

        [TestMethod]
        public void VariableParserTest_Level88()
        {
            var strings = new List<string>
            {
                " 88 FOO-1-BAR VALUE 1.                           ",
                " 88 FOO-1-BAR VALUE \"@\".                       ",
                " 88 FOO-1-BAR VALUE IS 1.                        ",
                " 88 FOO-1-BAR VALUE IS ZERO.                     ",
                " 88 FOO-1-BAR VALUE IS ZEROS.                    ",
                " 88 FOO-1-BAR VALUE IS ZEROES.                   ",
                " 88 FOO-1-BAR VALUE IS SPACE.                    ",
                " 88 FOO-1-BAR VALUE IS SPACES.                   ",
                " 88 FOO-1-BAR VALUE IS HIGH-VALUES.              ",
                " 88 FOO-1-BAR VALUE IS LOW-VALUES.               ",
                " 88 FOO-1-BAR VALUES ARE HIGH-VALUES.            ",
                " 88 FOO-1-BAR VALUES ARE LOW-VALUES.             ",
                " 88 FOO-1-BAR VALUES ARE ZEROS.                  ",
                " 88 FOO-1-BAR VALUES ARE ZEROES.                 ",
                " 88 FOO-1-BAR VALUES ARE HIGH-VALUES THROUGH 5.  ",
                " 88 FOO-1-BAR VALUES ARE HIGH-VALUES THRU 5.     ",
                " 88 FOO-1-BAR VALUES HIGH-VALUES THROUGH 5.      ",
                " 88 FOO-1-BAR VALUES HIGH-VALUES THRU 5.         ",
                " 88 FOO-1-BAR VALUES ARE 4 THROUGH 6.            ",
                " 88 FOO-1-BAR VALUES ARE 4 THRU 6.               ",
                " 88 FOO-1-BAR VALUES 4 THRU 6.                   ",
                " 88 FOO-1-BAR VALUE \"foo\", \"bar\".            "
            };

            foreach (var s in strings)
            {
                var actual = Canal.Utils.VariablesUtil.AnalyzeVariables(s);

                Assert.AreEqual(1, actual.Count, s);
            }
        }
    }
}
