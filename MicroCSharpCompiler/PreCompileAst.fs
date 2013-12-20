module PreCompileAst

open Ast
open System
open System.Reflection
open System.Reflection.Emit

type PcFile = 
| PcFile of string list * PcNamespaceBody list
and PcNamespaceBody = 
| PcInterface of TypeBuilder * InterfaceBody list
| PcClass of TypeBuilder
| PcStruct of TypeBuilder
| PcEnum of Type 

let accessModifierToTypeAttribute = function
| Public -> TypeAttributes.Public
| Private -> failwith "Protected is not valid on Types"
| Internal -> TypeAttributes.NotPublic
| Protected -> failwith "Protected is not valid on Types"

let (|CLASS|_|) (mb:ModuleBuilder, body:NamespaceBody, namespaceName) = 
    match body with
    | Class(name, visibility) -> Some(mb.DefineType(namespaceName+"."+name, accessModifierToTypeAttribute visibility))
    | _ -> None

let (|INTERFACE|_|) (mb:ModuleBuilder, body:NamespaceBody, namespaceName) = 
    match body with
    | Interface(name, visibility, body) -> 
        Some(mb.DefineType(namespaceName+"."+name, TypeAttributes.Abstract ||| TypeAttributes.Interface ||| accessModifierToTypeAttribute visibility), body)
    | _ -> None

//Rule break here - I just compile the Enums here - no point doing a 2 pass I think - maybe I'll change my mind later
let (|ENUM|_|) (mb:ModuleBuilder, body:NamespaceBody, namespaceName) = 
    match body with
    | Enum(name, visibility, values) -> 
        let eb = mb.DefineEnum(namespaceName+"."+name, accessModifierToTypeAttribute visibility, typeof<int>)
        values|>List.mapi(fun i n -> eb.DefineLiteral(n,i))|>ignore
        Some(eb.CreateType())
    | _ -> None

let compileType mb namespaceBody namespaceName =
    match (mb, namespaceBody, namespaceName) with
    | CLASS(tb) -> Some(PcClass(tb))
    | INTERFACE(tb, body) -> Some(PcInterface(tb, body)) 
    | ENUM(tb) -> Some(PcEnum(tb))
    | _ -> failwith "Unrecognized Namespace Body"

let preCompile (ast: File) filename = 
    let aName = AssemblyName(filename)
    let ab = AppDomain.CurrentDomain.DefineDynamicAssembly(aName, AssemblyBuilderAccess.Save);
    let mb = ab.DefineDynamicModule(aName.Name, aName.Name + ".dll");
    match ast with
    | File fileBody -> 
        let usings = fileBody |> List.choose(fun x -> match x with Using(using) -> Some(using) | _ -> None)
        let namespaces = fileBody |> List.choose(fun x -> match x with Namespace(name, bodyList) -> Some(name, bodyList) | _ -> None)
        ab, PcFile(usings, 
                    namespaces
                    |>List.collect(fun (name, body) -> body|>List.choose(fun b -> compileType mb b name)))
        