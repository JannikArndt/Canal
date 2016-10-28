using System;

namespace Model.Exceptions
{
    public class CopiedRessourceNotFoundException : Exception
    {
        public string Filename { get; set; }

        public CopiedRessourceNotFoundException(string filename) : base("The file " + filename + " has no occurs in the file cache and could therefore not be linked!")
        {
            Filename = filename;
        }
    }
}