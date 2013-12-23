module PreCompileAst

open Ast
open System
open System.Reflection
open System.Reflection.Emit

type PcFile = 
| PcFile of string list * PcNamespaceBody list
and PcNamespaceBody = 
| PcInterface of TypeBuilder * PcInterfaceBody list
| PcClass of TypeBuilder * PcClassBody list
| PcStruct of TypeBuilder
| PcEnum of Type 
and PcInterfaceBody =
| PcMethod of Type option * Name * (Type * Name) list
and PcClassBody = 
| PcMethod of AccessModifier * Type option * Name * (Type * Name) list * Expr list 

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

//TODO - lookup BCL types
let resolveType name usings = 
    match name with
    | "void" -> None
    | "int" -> Some(typeof<int>)
    | "float" -> Some(typeof<float32>)
    | "double" -> Some(typeof<float>)
    | "string" -> Some(typeof<string>)
    | "bool" -> Some(typeof<bool>)
    | _ -> failwith "Unrecognized type"

    
//TODO - walk all referenced assemblies
let getTypeByName name = 
    let t = Type.GetType name
    t

let (|CLASS|_|) (mb:ModuleBuilder, body:NamespaceBody, namespaceName, usings) = 
    match body with
    | Class(name, visibility, body) -> 
        let definedType = mb.DefineType(namespaceName+"."+name, accessModifierToTypeAttribute visibility)
        let body = body 
                   |> List.map(fun (ClassBody.Method(acccessModifier, typename, name, parameters, exprList)) -> 
                                        let returnType = resolveType typename usings
                                        let parameters = parameters
                                                         |>List.map(fun (Parameter(typename, name)) -> (resolveType typename usings).Value, name) //todo - dont use .Value
                                        PcClassBody.PcMethod(acccessModifier, returnType, name, parameters, exprList))
        Some(definedType, body)
    | _ -> None

let (|INTERFACE|_|) (mb:ModuleBuilder, body:NamespaceBody, namespaceName, usings) = 
    match body with
    | Interface(name, visibility, body) -> 
        let definedType = mb.DefineType(namespaceName+"."+name, TypeAttributes.Abstract ||| TypeAttributes.Interface ||| accessModifierToTypeAttribute visibility)
        let body = body 
                   |> List.map(fun (Method(typename, name, parameters)) -> 
                                        let returnType = resolveType typename usings
                                        let parameters = parameters
                                                         |>List.map(fun (Parameter(typename, name)) -> (resolveType typename usings).Value, name) //todo - dont use .Value
                                        PcInterfaceBody.PcMethod(returnType, name, parameters))
        Some(definedType, body)
    | _ -> None

//Rule break here - I just compile the Enums here - no point doing a 2 pass I think - maybe I'll change my mind later
let (|ENUM|_|) (mb:ModuleBuilder, body:NamespaceBody, namespaceName, usings) = 
    match body with
    | Enum(name, visibility, values) -> 
        let eb = mb.DefineEnum(namespaceName+"."+name, accessModifierToTypeAttribute visibility, typeof<int>)
        values|>List.mapi(fun i n -> eb.DefineLiteral(n,i))|>ignore
        Some(eb.CreateType())
    | _ -> None

let compileType mb namespaceBody namespaceName usings =
    match (mb, namespaceBody, namespaceName, usings) with
    | CLASS(tb) -> Some(PcClass(tb))
    | INTERFACE(tb, body) -> Some(PcInterface(tb, body)) 
    | ENUM(tb) -> Some(PcEnum(tb))
    | _ -> failwith "Unrecognized Namespace Body"

let preCompile (ast: File) filename = 
    let aName = AssemblyName(filename)
    let ab = AppDomain.CurrentDomain.DefineDynamicAssembly(aName, AssemblyBuilderAccess.Save)
    let mb = ab.DefineDynamicModule(aName.Name, aName.Name + ".dll")
    match ast with
    | File fileBody -> 
        let usings = fileBody |> List.choose(fun x -> match x with Using(using) -> Some(using) | _ -> None)
        let namespaces = fileBody |> List.choose(fun x -> match x with Namespace(name, bodyList) -> Some(name, bodyList) | _ -> None)
        ab, PcFile(usings, 
                    namespaces
                    |>List.collect(fun (name, body) -> body|>List.choose(fun b -> compileType mb b name usings)))
        