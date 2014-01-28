module Struct

open Definitions
open Reflection

open System.Reflection.Emit

let emptyStruct typeBuilder ast = { StructDefinition.Type = typeBuilder }
  //  ; Constructors = []; Fields = []; Properties = []; Methods = []; Ast = ast }

let buildStruct (moduleBuilder:ModuleBuilder) namespaceName name access body = 
    let definedType = moduleBuilder.DefineType(namespaceName+"."+name, accessModifierToTypeAttribute access,typeof<System.ValueType>)
    emptyStruct definedType body

let (|BuildStruct|_|) (namespaceName, body, moduleBuilder) = 
    match body with
    | Ast.Struct(name, access) as ast -> Some(buildStruct moduleBuilder namespaceName name access body)
    | _ -> None