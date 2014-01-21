module Interface

open Reflection

open System.Reflection
open System.Reflection.Emit

type InterfaceDefinition = {
    Type : TypeBuilder
    Properties : PropertyBuilder list
    Methods : MethodBuilder list
}

let emptyInterface typeBuilder = { Type = typeBuilder; Properties = []; Methods = [] }
 
let buildInterface (moduleBuilder:ModuleBuilder) namespaceName name access body = 
    let definedType = moduleBuilder.DefineType(namespaceName+"."+name, TypeAttributes.Abstract ||| TypeAttributes.Interface ||| accessModifierToTypeAttribute access)
    emptyInterface definedType

let (|BuildInterface|_|) (namespaceName, body, moduleBuilder) = 
    match body with
    | Ast.Interface(name, access, body) -> Some(buildInterface moduleBuilder namespaceName name access body)
    | _ -> None