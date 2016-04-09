using System;

namespace Canal.Utils
{
    public class CobolTreeException : Exception
    {
        public CobolTreeException()
        {
        }

        public CobolTreeException(string message, params string[] args)
            : base(string.Format(message, args))
        {
        }
    }
}
