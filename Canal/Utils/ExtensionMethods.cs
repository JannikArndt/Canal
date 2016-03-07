using System.Collections.Generic;
using System.Linq;

namespace Canal.Utils
{
    using Canal.CobolTree;

    public static class ExtensionMethods
    {
        public static Variable FindVariable(this List<Variable> list, string name)
        {
            foreach (var variable in list)
            {
                var result = list.FirstOrDefault(v => v.Name == name);

                if (result != null) return result;

                result = variable.Variables.FindVariable(name);

                if (result != null) return result;
            }

            return null;
        }
    }
}
