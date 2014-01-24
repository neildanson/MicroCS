module Reflection

open Ast
open System.Reflection

let accessModifierToTypeAttribute = function
    | Public -> TypeAttributes.Public
    | Private -> failwith "Protected is not valid on Types"
    | Internal -> TypeAttributes.NotPublic
    | Protected -> failwith "Protected is not valid on Types"

let accessModifierToMethodAttribute = function
    | Public -> MethodAttributes.Public
    | Private -> MethodAttributes.Private
    | Internal -> MethodAttributes.Assembly
    | Protected -> MethodAttributes.Family

let accessModifierToFieldAttribute = function
    | Public -> FieldAttributes.Public
    | Private -> FieldAttributes.Private
    | Internal -> FieldAttributes.Assembly
    | Protected -> FieldAttributes.Family