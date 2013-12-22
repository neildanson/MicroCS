﻿namespace Ast
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
| Class of Name * AccessModifier * ClassBody list
| Struct of Name * AccessModifier
| Enum of Name * AccessModifier * Name list //Should this be Name*int for custom int values?
and ClassBody = 
| Field of Name
| Property of Name
| Event of Name
| Method of AccessModifier * TypeName * Name * Parameter list * Expr list //TODO AccessModifier
and InterfaceBody =
| Method of TypeName * Name * Parameter list
//| Property, Event, yada yada
and Parameter = 
| Parameter of TypeName * Name
and Expr =
| String of string
| Int of int
| Float of float32
| Double of float
| Call of Name * Expr list

module AstHelpers = 
    let toAccessModifier = function
    | "public" -> Public
    | "private" -> Private
    | "internal" -> Internal
    | "protected" -> Protected
    | _ -> failwith "Unidentified Visibility"