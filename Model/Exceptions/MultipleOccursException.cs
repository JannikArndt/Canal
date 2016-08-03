using System;

namespace Model.Exceptions
{
    public class MultipleOccursException : Exception
    {
        public MultipleOccursException(string line) : base("Error parsing variable: cascading occurs occured in line '" + line + "'.")
        {
        }
    }
}