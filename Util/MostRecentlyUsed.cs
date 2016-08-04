using Logging;
using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.IO;
using System.Linq;
using System.Runtime.Serialization;

namespace Util
{
    public class MostRecentlyUsed
    {
        public static MostRecentlyUsed Instance = new MostRecentlyUsed();

        private Dictionary<string, DateTime> _files = new Dictionary<string, DateTime>();

        private string filename = "mostRecentlyUsed.canalsettings";

        private MostRecentlyUsed()
        {
            Load();
        }

        public void Trim()
        {
            if (_files == null || _files.None())
                return;

            _files = _files.OrderByDescending(file => file.Value).Take(20).ToDictionary(item => item.Key, item => item.Value);
            Save();
        }

        public void Add(string file)
        {
            if (_files.ContainsKey(file))
                _files[file] = DateTime.Now;
            else
                _files.Add(file, DateTime.Now);
            Save();
        }

        public List<string> GetFiles()
        {
            if (_files == null || _files.None())
                return new List<string>();

            Trim();
            return _files.OrderByDescending(file => file.Value).Select(item => item.Key).ToList();
        }

        public string GetMostRecentFile()
        {
            if (_files == null || _files.None())
                return string.Empty;

            return _files.OrderByDescending(file => file.Value).First().Key;
        }

        public void Import(StringCollection oldList)
        {
            foreach (var oldName in oldList)
            {
                Add(oldName);
            }
        }

        private void Save()
        {
            try
            {
                using (var fileStream = new FileStream(filename, FileMode.Create, FileAccess.ReadWrite))
                {
                    var ser = new DataContractSerializer(typeof(Dictionary<string, DateTime>));
                    ser.WriteObject(fileStream, _files);
                }

            }
            catch (Exception exception)
            {
                Logger.Error("Error trying to save most recently used files at path {0}: {1}", filename, exception.Message);
            }

        }

        private void Load()
        {
            try
            {
                using (var fileStream = new FileStream(filename, FileMode.Open, FileAccess.Read))
                {
                    var ser = new DataContractSerializer(typeof(Dictionary<string, DateTime>));
                    _files = (Dictionary<string, DateTime>)ser.ReadObject(fileStream);
                }
            }
            catch (Exception exception)
            {
                Logger.Error("Error trying to open most recently used files at path {0}: {1}", filename, exception.Message);
            }
        }
    }
}
