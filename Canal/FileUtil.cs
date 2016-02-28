using System.IO;

namespace Canal
{
    public static class FileUtil
    {
        public static CobolFile Get(string filename)
        {
            var lines = File.ReadLines(filename);

            var file = new CobolFile(lines);

            return file;
        }
    }
}
