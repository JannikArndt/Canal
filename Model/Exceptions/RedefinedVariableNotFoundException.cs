namespace Model.Exceptions
{
    public class RedefinedVariableNotFoundException : CanalException
    {
        public RedefinedVariableNotFoundException(string valRedefines, string line) :
            base("Error parsing variable line '" + line + "': Variable '" + valRedefines + "' not found in Dictionary.")
        {
        }
    }
}