using System;

namespace Model.Exceptions
{
    public class CopiedRessourceNotFoundException : CanalException
    {
        public string Filename { get; set; }

        public CopiedRessourceNotFoundException(string filename) : base("The file " + filename + " could not be found.")
        {
            Filename = filename;
        }
    }
}