module Compiler

open System
open System.Reflection
open System.Reflection.Emit

open Ast

//Things to consider:
//Mutually recursive types
//EnumBuilder and TypeBuilder both need to CreateType, but method defined seperately
//2 pass approach? define types and collect, then define bodies

//todo lok up built types and BCL types
let resolveType name (*usings*) = 
    match name with
    | "void" -> None
    | "int" -> Some(typeof<int>)
    | "float" -> Some(typeof<float32>)
    | "double" -> Some(typeof<float>)
    | "string" -> Some(typeof<string>)
    | _ -> failwith "Unrecognized type"

let VisibilityToTypeAttribute = function
| Public -> TypeAttributes.Public
| Private -> failwith "Protected is not valid on Types"
| Internal -> TypeAttributes.NotPublic
| Protected -> failwith "Protected is not valid on Types"


let (|CLASS|_|) (mb:ModuleBuilder, body:NamespaceBody, namespaceName) = 
    match body with
    | Class(name, visibility) -> let tb = mb.DefineType(namespaceName+"."+name, VisibilityToTypeAttribute visibility)
                                 Some(tb)
    | _ -> None

let (|INTERFACE|_|) (mb:ModuleBuilder, body:NamespaceBody, namespaceName) = 
    match body with
    | Interface(name, visibility, body) -> 
        let tb = mb.DefineType(namespaceName+"."+name, TypeAttributes.Abstract ||| TypeAttributes.Interface ||| VisibilityToTypeAttribute visibility)
        body|>List.iter(fun (Method(returnType, name)) -> 
            ignore<| match resolveType returnType with
                     | Some(returnType) -> tb.DefineMethod(name, MethodAttributes.Abstract ||| MethodAttributes.Virtual, returnType, [||]) 
                     | None ->  tb.DefineMethod(name, MethodAttributes.Abstract ||| MethodAttributes.Virtual))
        Some(tb)
    | _ -> None

let (|ENUM|_|) (mb:ModuleBuilder, body:NamespaceBody, namespaceName) = 
    match body with
    | Enum(name, visibility, values) -> 
        let eb = mb.DefineEnum(namespaceName+"."+name, VisibilityToTypeAttribute visibility, typeof<int>)
        values|>List.mapi(fun i n -> eb.DefineLiteral(n,i))|>ignore
        eb.CreateType() |> ignore
        Some(eb)
    | _ -> None

let compileType mb namespaceBody namespaceName=
    match (mb, namespaceBody, namespaceName) with
    | CLASS(tb) -> Some(tb)
    | INTERFACE(tb) -> Some(tb) 
    | ENUM(tb) -> None
    | _ -> failwith "Unrecognized Namespace Body"


let compile (ast: Ast.File) = 
    let aName = AssemblyName("CSharpCompilerExample")
    let ab = AppDomain.CurrentDomain.DefineDynamicAssembly(aName, AssemblyBuilderAccess.RunAndSave);
    let mb = ab.DefineDynamicModule(aName.Name, aName.Name + ".dll");
    match ast with
    | File fileBody -> 
        fileBody |> List.filter(fun fb -> match fb with Namespace(_,_) -> true | _-> false) 
                 |> List.iter(fun (Namespace(name, bodyList)) -> bodyList|>List.map(fun ns -> compileType mb ns name)|>List.choose(id) |> List.iter(fun tb -> tb.CreateType() |> ignore))

    

    ab.Save(aName.Name + ".dll")

