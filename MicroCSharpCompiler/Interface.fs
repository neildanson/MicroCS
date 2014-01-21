module Interface

open Reflection
open Definitions

open System.Reflection
open System.Reflection.Emit

let emptyInterface typeBuilder = { InterfaceDefinition.Type = typeBuilder; Properties = []; Methods = [] }
 
let buildInterface (moduleBuilder:ModuleBuilder) namespaceName name access body = 
    let definedType = moduleBuilder.DefineType(namespaceName+"."+name, TypeAttributes.Abstract ||| TypeAttributes.Interface ||| accessModifierToTypeAttribute access)
    emptyInterface definedType

let (|BuildInterface|_|) (namespaceName, body, moduleBuilder) = 
    match body with
    | Ast.Interface(name, access, body) -> Some(buildInterface moduleBuilder namespaceName name access body)
    | _ -> None