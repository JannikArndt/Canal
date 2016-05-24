using System;

namespace Util.Exceptions
{
    public class RedefinedVariableNotFoundException : Exception
    {
        public RedefinedVariableNotFoundException(string valRedefines, string line) :
            base("Error parsing variable line '" + line + "': Variable '" + valRedefines + "' not found in Dictionary.")
        {
        }
    }
}