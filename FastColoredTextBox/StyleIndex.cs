using System;

namespace FastColoredTextBoxNS
{
    /// <summary>
    /// Style index mask (16 styles)
    /// </summary>
    [Flags]
    public enum StyleIndex : ushort
    {
        None = 0,
        Style0 = 0x1,
        Style1 = 0x2,
        Style2 = 0x4,
        Style3 = 0x8,
        Style4 = 0x10,
        Style5 = 0x20,
        Style6 = 0x40,
        Style7 = 0x80,
        Style8 = 0x100,
        Style9 = 0x200,
        Style10 = 0x400,
        Style11 = 0x800,
        Style12 = 0x1000,
        Style13 = 0x2000,
        Style14 = 0x4000,
        Style15 = 0x8000,
        All = 0xffff
    }
}