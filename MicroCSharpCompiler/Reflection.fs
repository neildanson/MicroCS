module Reflection

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

let resolveTypeIntegral name =
    match name with
    | "void" -> typeof<System.Void> |> Some
    | "int" -> typeof<int> |> Some
    | "float" -> typeof<float32> |> Some
    | "double" -> typeof<float> |> Some
    | "string" -> typeof<string> |> Some
    | "bool" -> typeof<bool> |> Some
    | "decimal" -> typeof<decimal> |> Some
    | "object" -> typeof<obj> |> Some
    | _ -> None //TODO other integral types