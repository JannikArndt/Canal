using System;
using System.ComponentModel;

namespace FastColoredTextBoxNS
{
    class FooTextChangedDescriptor : EventDescriptor
    {
        public FooTextChangedDescriptor(MemberDescriptor desc)
            : base(desc)
        {
        }

        public override void AddEventHandler(object component, Delegate value)
        {
            (component as FastColoredTextBox).BindingTextChanged += value as EventHandler;
        }

        public override Type ComponentType
        {
            get { return typeof(FastColoredTextBox); }
        }

        public override Type EventType
        {
            get { return typeof(EventHandler); }
        }

        public override bool IsMulticast
        {
            get { return true; }
        }

        public override void RemoveEventHandler(object component, Delegate value)
        {
            (component as FastColoredTextBox).BindingTextChanged -= value as EventHandler;
        }
    }
}