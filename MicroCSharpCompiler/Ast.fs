namespace Ast
open System

type Name = string
type TypeName = string
type AccessModifier = Public | Private | Internal | Protected

type File = 
| File of FileBody list
and FileBody = 
| Using of string
| Namespace of Name * NamespaceBody list
and NamespaceBody = 
| Interface of Name * AccessModifier * InterfaceBody list
| Class of Name * AccessModifier
| Struct of Name * AccessModifier
| Enum of Name * AccessModifier * Name list //Should this be Name*int for custom int values?
and ClassBody = 
| Field of Name
| Property of Name
| Event of Name
| Method of Name
and InterfaceBody =
| Method of TypeName * Name * Parameter list
//| Property, Event, yada yada
and Parameter = 
| Parameter of TypeName * Name

module AstHelpers = 
    let toAccessModifier = function
    | "public" -> Public
    | "private" -> Private
    | "internal" -> Internal
    | "protected" -> Protected
    | _ -> failwith "Unidentified Visibility"