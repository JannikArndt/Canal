﻿//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//     Runtime Version:4.0.30319.42000
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------

namespace CodeGenerator.Properties {
    using System;
    
    
    /// <summary>
    ///   A strongly-typed resource class, for looking up localized strings, etc.
    /// </summary>
    // This class was auto-generated by the StronglyTypedResourceBuilder
    // class via a tool like ResGen or Visual Studio.
    // To add or remove a member, edit your .ResX file then rerun ResGen
    // with the /str option, or rebuild your VS project.
    [global::System.CodeDom.Compiler.GeneratedCodeAttribute("System.Resources.Tools.StronglyTypedResourceBuilder", "4.0.0.0")]
    [global::System.Diagnostics.DebuggerNonUserCodeAttribute()]
    [global::System.Runtime.CompilerServices.CompilerGeneratedAttribute()]
    internal class Resources {
        
        private static global::System.Resources.ResourceManager resourceMan;
        
        private static global::System.Globalization.CultureInfo resourceCulture;
        
        [global::System.Diagnostics.CodeAnalysis.SuppressMessageAttribute("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
        internal Resources() {
        }
        
        /// <summary>
        ///   Returns the cached ResourceManager instance used by this class.
        /// </summary>
        [global::System.ComponentModel.EditorBrowsableAttribute(global::System.ComponentModel.EditorBrowsableState.Advanced)]
        internal static global::System.Resources.ResourceManager ResourceManager {
            get {
                if (object.ReferenceEquals(resourceMan, null)) {
                    global::System.Resources.ResourceManager temp = new global::System.Resources.ResourceManager("CodeGenerator.Properties.Resources", typeof(Resources).Assembly);
                    resourceMan = temp;
                }
                return resourceMan;
            }
        }
        
        /// <summary>
        ///   Overrides the current thread's CurrentUICulture property for all
        ///   resource lookups using this strongly typed resource class.
        /// </summary>
        [global::System.ComponentModel.EditorBrowsableAttribute(global::System.ComponentModel.EditorBrowsableState.Advanced)]
        internal static global::System.Globalization.CultureInfo Culture {
            get {
                return resourceCulture;
            }
            set {
                resourceCulture = value;
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to namespace &lt;!NAMESPACE!&gt;
        ///{
        ///    public class &lt;!BUSINESSOBJECTNAME!&gt; : IBusinessObject
        ///    {       
        ///&lt;!PROPERTIES!&gt;
        ///
        ///        public &lt;!BUSINESSOBJECTNAME!&gt;()
        ///        {
        ///
        ///        }
        ///    }
        ///}
        ///.
        /// </summary>
        internal static string BusinessObjectExample {
            get {
                return ResourceManager.GetString("BusinessObjectExample", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to using System;
        ///
        ///namespace CodeGenerator.Resources
        ///{
        ///    public static class ByteArrayExtemsions
        ///    {
        ///        public static string GetString(this byte[] bytes, int offset, int length)
        ///        {
        ///            return &quot;&quot;;
        ///        }
        ///
        ///        public static void SetString(this byte[] bytes, int offset, int length, string text)
        ///        {
        ///
        ///        }
        ///
        ///        public static byte[] GetBytes(this byte[] bytes, int offset, int length)
        ///        {
        ///            var result = new byte[length];
        ///            Array [rest of string was truncated]&quot;;.
        /// </summary>
        internal static string ByteArrayExtensions {
            get {
                return ResourceManager.GetString("ByteArrayExtensions", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to namespace CodeGenerator.Resources
        ///{
        ///    public enum EnumExample
        ///    {
        ///        Foo,
        ///        Bar
        ///    }
        ///}
        ///.
        /// </summary>
        internal static string EnumExample {
            get {
                return ResourceManager.GetString("EnumExample", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to namespace CodeGenerator.Resources
        ///{
        ///    public interface IBusinessObject
        ///    {
        ///    }
        ///}
        ///.
        /// </summary>
        internal static string IBusinessObject {
            get {
                return ResourceManager.GetString("IBusinessObject", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to namespace CodeGenerator.Resources
        ///{
        ///    /// &lt;summary&gt;
        ///    /// Mapper Interface
        ///    /// &lt;/summary&gt;
        ///    public interface IMapper&lt;TBusinessObjectType&gt;
        ///                where TBusinessObjectType : IBusinessObject, new()
        ///    {
        ///        /// &lt;summary&gt;
        ///        /// Maps a byte array to a business object.
        ///        /// &lt;/summary&gt;
        ///        /// &lt;param name=&quot;bytes&quot;&gt;A byte array&lt;/param&gt;
        ///        /// &lt;returns&gt;A business object&lt;/returns&gt;
        ///        TBusinessObjectType Map(byte[] bytes);
        ///
        ///        /// &lt;summary&gt;
        ///       [rest of string was truncated]&quot;;.
        /// </summary>
        internal static string IMapper {
            get {
                return ResourceManager.GetString("IMapper", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized resource of type System.Drawing.Bitmap.
        /// </summary>
        internal static System.Drawing.Bitmap LoadConfig {
            get {
                object obj = ResourceManager.GetObject("LoadConfig", resourceCulture);
                return ((System.Drawing.Bitmap)(obj));
            }
        }
        
        /// <summary>
        ///   Looks up a localized resource of type System.Drawing.Bitmap.
        /// </summary>
        internal static System.Drawing.Bitmap LoadSettings {
            get {
                object obj = ResourceManager.GetObject("LoadSettings", resourceCulture);
                return ((System.Drawing.Bitmap)(obj));
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to namespace &lt;!NAMESPACE!&gt;
        ///{
        ///    /// &lt;summary&gt;
        ///    /// Mapper from Byte Array to &lt;!BUSINESSOBJECTNAME!&gt; and back.
        ///    /// &lt;/summary&gt;
        ///    public class &lt;!BUSINESSOBJECTNAME!&gt;Mapper : IMapper&lt;&lt;!BUSINESSOBJECTNAME!&gt;&gt;
        ///    {
        ///        /// &lt;summary&gt;
        ///        /// Maps a byte array to a &lt;!BUSINESSOBJECTNAME!&gt;.
        ///        /// &lt;/summary&gt;
        ///        /// &lt;param name=&quot;bytes&quot;&gt;A byte array&lt;/param&gt;
        ///        /// &lt;returns&gt;A &lt;!BUSINESSOBJECTNAME!&gt;&lt;/returns&gt;
        ///        public &lt;!BUSINESSOBJECTNAME!&gt; Map(byte[] bytes)
        ///        {
        ///    [rest of string was truncated]&quot;;.
        /// </summary>
        internal static string MapperExample {
            get {
                return ResourceManager.GetString("MapperExample", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized resource of type System.Drawing.Bitmap.
        /// </summary>
        internal static System.Drawing.Bitmap SaveConfig {
            get {
                object obj = ResourceManager.GetObject("SaveConfig", resourceCulture);
                return ((System.Drawing.Bitmap)(obj));
            }
        }
        
        /// <summary>
        ///   Looks up a localized resource of type System.Drawing.Bitmap.
        /// </summary>
        internal static System.Drawing.Bitmap SaveSettings {
            get {
                object obj = ResourceManager.GetObject("SaveSettings", resourceCulture);
                return ((System.Drawing.Bitmap)(obj));
            }
        }
    }
}
