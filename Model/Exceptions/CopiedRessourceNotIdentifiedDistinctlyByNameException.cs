using System;

namespace Model.Exceptions
{
    public class CopiedRessourceNotIdentifiedDistinctlyByNameException : Exception
    {
        public string Filename { get; set; }

        public CopiedRessourceNotIdentifiedDistinctlyByNameException(string filename) : base("The file " + filename + " occurs mutliple times in the file cache and could therefore not be linked!")
        {
            Filename = filename;
        }
    }
}