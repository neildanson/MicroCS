module Compiler

open System
open System.Reflection
open System.Reflection.Emit

open Ast
open PreCompileAst

//Things to consider:
//EnumBuilder and TypeBuilder both need to CreateType, but method defined seperately


//todo lok up built types and BCL types
let resolveType name usings = 
    match name with
    | "void" -> None
    | "int" -> Some(typeof<int>)
    | "float" -> Some(typeof<float32>)
    | "double" -> Some(typeof<float>)
    | "string" -> Some(typeof<string>)
    | _ -> failwith "Unrecognized type"

let compileInterface (tb:TypeBuilder) body usings= 
    body|>List.iter(fun (Method(returnType, name, parameters)) -> 
                    ignore<| match resolveType returnType usings with
                             | Some(returnType) -> tb.DefineMethod(name, MethodAttributes.Abstract ||| MethodAttributes.Virtual, returnType, 
                                                                   parameters|>List.choose(fun (Parameter(typeName, name)) -> resolveType typeName usings)|>List.toArray) 
                             | None ->  tb.DefineMethod(name, MethodAttributes.Abstract ||| MethodAttributes.Virtual))
    tb

let compileType usings = function
    | PcInterface(tb, body) -> Some(compileInterface tb body usings)
    | PcClass(tb)
    | PcStruct(tb) -> Some(tb)
    | PcEnum(_) -> None (*enums already compiled*)

let compileFile(PcFile(usings, body)) = 
    body
    |>List.choose(fun b -> compileType usings b)
    |>List.iter(fun tb -> tb.CreateType() |> ignore)

let compile (ast: PcFile) = 
    compileFile ast

