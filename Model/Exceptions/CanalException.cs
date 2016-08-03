using System;
using System.Runtime.Serialization;

namespace Model.Exceptions
{
    public class CanalException : Exception
    {
        public CanalException()
        {
        }

        public CanalException(string message) : base(message)
        {
        }

        public CanalException(string message, Exception innerException) : base(message, innerException)
        {

        }

        protected CanalException(SerializationInfo info, StreamingContext context) : base(info, context)
        {
        }
    }
}