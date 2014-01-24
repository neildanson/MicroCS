module Definitions

open Ast

open System
open System.Reflection
open System.Reflection.Emit

type MethodSignature = Type array
type UserDefinitions = ClassDefinition list * InterfaceDefinition list * StructDefinition list * EnumBuilder list
and ClassDefinition = {
    Type : TypeBuilder
    Constructors : ConstructorBuilder list
    Fields : FieldInfo list
    Properties : PropertyBuilder list
    Methods : (MethodBuilder * MethodSignature * (UserDefinitions -> unit)) list 
    Ast : ClassBody list
}
and InterfaceDefinition = {
    Type : TypeBuilder
    Properties : PropertyBuilder list
    Methods : MethodBuilder list
    Ast : InterfaceBody list
}
and StructDefinition = { 
    Type : TypeBuilder
}
