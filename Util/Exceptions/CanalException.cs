using System;

namespace Util.Exceptions
{
    public class CanalException : Exception
    {
        public CanalException(string message, Exception innerException) : base(message, innerException)
        {

        }
    }
}