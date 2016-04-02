using System;

namespace CodeGenerator.Resources
{
    public static class ByteArrayExtemsions
    {
        public static string GetString(this byte[] bytes, int offset, int length)
        {
            return "";
        }

        public static void SetString(this byte[] bytes, int offset, int length, string text)
        {

        }

        public static byte[] GetBytes(this byte[] bytes, int offset, int length)
        {
            var result = new byte[length];
            Array.Copy(bytes, offset, result, length);
            return result;
        }
    }
}
