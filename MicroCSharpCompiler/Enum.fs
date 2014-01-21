module Enum

open Definitions
open Reflection

open System.Reflection
open System.Reflection.Emit

let buildEnum (moduleBuilder:ModuleBuilder) namespaceName name access values = 
    let eb = moduleBuilder.DefineEnum(namespaceName+"."+name, accessModifierToTypeAttribute access, typeof<int>)
    values|>List.mapi(fun i n -> eb.DefineLiteral(n,i))|>ignore
    eb
        

let (|BuildEnum|_|) (namespaceName, body, moduleBuilder) = 
    match body with
    | Ast.Enum(name, access, values) -> Some(buildEnum moduleBuilder namespaceName name access values)
    | _ -> None

let (|GetEnum|_|) (userdefinitions:UserDefinitions,name) =
    let _,_,_,enums = userdefinitions
    enums|>List.tryFind(fun (eb:EnumBuilder) -> eb.Name = name)