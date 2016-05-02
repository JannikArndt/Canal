using CodeGenerator;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Tests
{
    [TestClass]
    public class StringExtensionsTests
    {
        private string text = "001234     EXIT.           \n" + // 0-27
                              "001234* FOO               \r\n" + // 28-55
                              "001234 LOGIK SECTION.      \n" + // 56-83
                              "001234 BEISPIEL-PROGRAMM-1.\n";  // 85-111

        [TestMethod]
        public void GetIndexOfCurrentLineStartTest()
        {
            Assert.AreEqual(0, text.GetIndexOfCurrentLineStart(0));
            Assert.AreEqual(0, text.GetIndexOfCurrentLineStart(2));
            Assert.AreEqual(0, text.GetIndexOfCurrentLineStart(27));
            Assert.AreEqual(28, text.GetIndexOfCurrentLineStart(28));
            Assert.AreEqual(28, text.GetIndexOfCurrentLineStart(40));
            Assert.AreEqual(28, text.GetIndexOfCurrentLineStart(54));
            Assert.AreEqual(28, text.GetIndexOfCurrentLineStart(55));
            Assert.AreEqual(56, text.GetIndexOfCurrentLineStart(56));
        }

        [TestMethod]
        public void GetIndexOfPreviousLineStartTest()
        {
            Assert.AreEqual(0, text.GetIndexOfPreviousLineStart(0));
            Assert.AreEqual(0, text.GetIndexOfPreviousLineStart(2));
            Assert.AreEqual(0, text.GetIndexOfPreviousLineStart(27));
            Assert.AreEqual(0, text.GetIndexOfPreviousLineStart(28));
            Assert.AreEqual(0, text.GetIndexOfPreviousLineStart(40));
            Assert.AreEqual(28, text.GetIndexOfPreviousLineStart(56));
            Assert.AreEqual(28, text.GetIndexOfPreviousLineStart(70));
        }

        [TestMethod]
        public void IsCommentLineTest()
        {
            Assert.IsFalse(text.IsCommentLine(0));
            Assert.IsFalse(text.IsCommentLine(2));
            Assert.IsFalse(text.IsCommentLine(27));
            Assert.IsTrue(text.IsCommentLine(28));
            Assert.IsTrue(text.IsCommentLine(30));
            Assert.IsTrue(text.IsCommentLine(55));
            Assert.IsFalse(text.IsCommentLine(56));
        }
    }
}
