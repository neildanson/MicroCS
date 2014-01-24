module Interface

open Ast
open Reflection
open Definitions

open System.Reflection
open System.Reflection.Emit

let emptyInterface typeBuilder ast = { InterfaceDefinition.Type = typeBuilder; Properties = []; Methods = [] ; Ast = ast}
 
let buildInterface (moduleBuilder:ModuleBuilder) namespaceName name access body = 
    let definedType = moduleBuilder.DefineType(namespaceName+"."+name, TypeAttributes.Abstract ||| TypeAttributes.Interface ||| accessModifierToTypeAttribute access)
    emptyInterface definedType body

let (|BuildInterface|_|) (namespaceName, body, moduleBuilder) = 
    match body with
    | Ast.Interface(name, access, body) -> Some(buildInterface moduleBuilder namespaceName name access body)
    | _ -> None

let WithMethod (interfaceDef:InterfaceDefinition) returnType name parameters resolveType = 
    let parameters = parameters |> List.map(fun (Parameter(typename, name)) -> resolveType typename, name) 
    let method' = interfaceDef.Type.DefineMethod(name, 
                               MethodAttributes.Virtual||| MethodAttributes.Abstract ||| MethodAttributes.Public, 
                               resolveType returnType, 
                               parameters|>List.map fst |> List.toArray)
    { interfaceDef with Methods = [method']@interfaceDef.Methods }