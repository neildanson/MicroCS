module Class

open Ast
open TypedAst //Hopefully remove this using and move the methods in here to elsewhere
open Reflection

open System
open System.Linq
open System.Reflection
open System.Reflection.Emit

type MethodSignature = Type array

type ClassDefinition = {
    Type : TypeBuilder
    Constructors : ConstructorBuilder list
    Fields : FieldBuilder list
    Properties : PropertyBuilder list
    Methods : (MethodBuilder * MethodSignature * (ClassDefinition list -> unit)) list //maybe need to move this definition so that we can reference other definitions
    Ast : ClassBody list
}

let emptyClass typeBuilder ast = { Type = typeBuilder; Constructors = []; Fields = []; Properties = []; Methods = []; Ast = ast }
 
let buildClass (moduleBuilder:ModuleBuilder) namespaceName name access body = 
    let definedType = moduleBuilder.DefineType(namespaceName+"."+name, accessModifierToTypeAttribute access)
    emptyClass definedType body

let (|BuildClass|_|) (namespaceName, body, moduleBuilder) = 
    match body with
    | Ast.Class(name, access, body) as ast -> Some(buildClass moduleBuilder namespaceName name access body)
    | _ -> None


let (|GetClass|_|) (userdefinitions, name) =
    let classes, _, _ = userdefinitions
    classes|>List.tryFind(fun (cd:ClassDefinition) -> cd.Type.Name = name || cd.Type.FullName = name)
    
let GetConstructor userdefined (t:Type, parameters)  = 
    match t with
    | :? TypeBuilder as t-> let classes = userdefined
                            let classDef = classes |> List.find(fun c -> c.Type = t)
                            let builder = classDef.Constructors
                                          |>List.find(fun c -> c.GetParameters() |> Array.map(fun (p:ParameterInfo) -> p.ParameterType) = parameters)
                            builder :> ConstructorInfo
    | :? System.Type as t -> t.GetConstructor(parameters)

let GetMethod userdefined (name, t:Type, parameters)  = 
    match t with
    | :? TypeBuilder as t-> let classes = userdefined
                            let classDef = classes |> List.find(fun c -> c.Type = t)
                            let builder,_,_ = classDef.Methods
                                              |>List.find(fun (m,s, _) -> m.Name = name && Enumerable.SequenceEqual(s,parameters))
                            builder :> MethodInfo
    | :? System.Type as t -> t.GetMethod(name, parameters)



let WithField classDef returnType name resolveType =
    let field = classDef.Type.DefineField(name, resolveType returnType, FieldAttributes.Private) //TODO Attributes
    { classDef with Fields = [field]@classDef.Fields }

let WithMethod classDef access returnType name parameters resolveType body = 
    let parameters = parameters |> List.map(fun (Parameter(typename, name)) -> resolveType typename, name) 
    let method' = classDef.Type.DefineMethod(name, 
                                             accessModifierToMethodAttribute access, 
                                             resolveType returnType, 
                                             parameters|>List.map fst |> List.toArray)
    let buildBody userdefined = 
        let variables = System.Collections.Generic.Dictionary<_,_>()
        let typed = body|> List.map(fun b -> toTypedExpr resolveType variables parameters classDef.Type (GetConstructor userdefined) (GetMethod userdefined) b)
        let il = method'.GetILGenerator()
        let parameters = parameters |> List.mapi(fun i (t,n) -> method'.DefineParameter(i+1, ParameterAttributes.In, n))
        typed|>List.fold(fun vars expr -> Compiler.eval il vars parameters expr) Map.empty |> ignore
        //Woder if I should check if ive already emitted a ret?
        il.Emit(OpCodes.Ret)

    { classDef with Methods = [method', parameters |> List.map fst |> List.toArray, buildBody]@classDef.Methods }