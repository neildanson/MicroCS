module Compiler

open System
open System.Reflection
open System.Reflection.Emit

open Ast
open PreCompileAst

//Things to consider:
//EnumBuilder and TypeBuilder both need to CreateType, but method defined seperately


//TODO - lookup BCL types
let resolveType name usings = 
    match name with
    | "void" -> None
    | "int" -> Some(typeof<int>)
    | "float" -> Some(typeof<float32>)
    | "double" -> Some(typeof<float>)
    | "string" -> Some(typeof<string>)
    | _ -> failwith "Unrecognized type"

    
//TODO - walk all referenced assemblies
let getTypeByName name = 
    let t = Type.GetType name
    t

let compileInterface (tb:TypeBuilder) body usings= 
    body|>List.iter(fun (Method(returnType, name, parameters)) -> 
                    ignore<| match resolveType returnType usings with
                             | Some(returnType) -> tb.DefineMethod(name, MethodAttributes.Abstract ||| MethodAttributes.Virtual, returnType, 
                                                                   parameters|>List.choose(fun (Parameter(typeName, name)) -> resolveType typeName usings)|>List.toArray) 
                             | None ->  tb.DefineMethod(name, MethodAttributes.Abstract ||| MethodAttributes.Virtual))
    tb


let rec eval (il:ILGenerator) = function
    | Call(name, parameters) -> 
        parameters|>List.iter(fun p -> eval il p)
        let className = name.Substring(0,name.LastIndexOf("."))
        let methodName = name.Substring(name.LastIndexOf(".")+1)
        System.Console.WriteLine(sprintf "c:%s m:%s" className methodName)
        let typeOf = getTypeByName className
        let mi = typeOf.GetMethod(methodName)
        il.EmitCall(OpCodes.Call, mi, null)  
    | String(s) -> il.Emit(OpCodes.Ldstr,s)
    | _ -> failwith "Currently unsupported"
let compileMethod (mb:MethodBuilder) (exprList:Expr list) usings = 
    let il = mb.GetILGenerator()
    usings|>List.iter (fun using -> il.UsingNamespace using)
    
    exprList|>List.iter(fun expr -> eval il expr)

    il.EmitWriteLine("Boo")
    

let compileClass (tb:TypeBuilder) body usings= 
    body|>List.iter(fun (ClassBody.Method(modifier, returnType, name, parameters, body)) -> 
                        compileMethod
                            (match resolveType returnType usings with
                             | Some(returnType) -> tb.DefineMethod(name, accessModifierToMethodAttribute modifier, returnType, 
                                                                   parameters|>List.choose(fun (Parameter(typeName, name)) -> resolveType typeName usings)|>List.toArray) 
                             | None ->  tb.DefineMethod(name, accessModifierToMethodAttribute modifier))  body usings)
    tb

let compileType usings = function
    | PcInterface(tb, body) -> Some(compileInterface tb body usings)
    | PcClass(tb, body) -> Some(compileClass tb body usings)
    | PcStruct(tb) -> Some(tb)
    | PcEnum(_) -> None (*enums already compiled*)

let compileFile(PcFile(usings, body)) = 
    body
    |>List.choose(fun b -> compileType usings b)
    |>List.iter(fun tb -> tb.CreateType() |> ignore)

let compile (ast: PcFile) = 
    compileFile ast

