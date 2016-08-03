using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Util
{
    public class RecentlyOpenedFile
    {
        public List<FileDatePair> Files { get; set; }

        public RecentlyOpenedFile()
        {
            Files = Files ?? new List<FileDatePair>();
        }

        public void Add(string filename)
        {
            Files.Add(new FileDatePair(filename, DateTime.Now));
        }

        public string GetMostRecentFile()
        {
            if (Files.None())
                return "";
            return Files?.OrderBy(file => file.DateTime).First()?.Filename;
        }

        public void Trim()
        {
            // Clean up
            Files = Files.DistinctBy(file => file.Filename).OrderBy(file => file.DateTime).Take(20).ToList();
            Properties.Settings.Default.Save();
        }
    }

    public class FileDatePair
    {
        public string Filename { get; set; }

        public DateTime DateTime { get; set; }

        public FileDatePair(string filename, DateTime dateTime)
        {
            Filename = filename;
            DateTime = dateTime;
        }

        public override string ToString()
        {
            return Filename;
        }
    }
}
