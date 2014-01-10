module TypedAst

open Ast
open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit

type TFile = 
| TFile of TNamespaceBody list
and TNamespaceBody = 
| TInterface of TypeBuilder * TInterfaceBody list
| TClass of TypeBuilder * TClassBody list
| TStruct of TypeBuilder
| TEnum of Type 
and TInterfaceBody =
| TMethod of Type option * Name * (Type * Name) list
and TClassBody = 
| TMethod of AccessModifier * Type option * Name * (Type * Name) list * TExpr list 
and TExpr = 
| TScope of TExpr list
| TVar of Type * Name
| TRef of Type * Name
//Are the following needed?
| TString of string
| TInt of int
| TFloat of float32
| TDouble of float
| TBool of bool
//End
| TInstanceCall of TExpr * MethodInfo * TExpr list
| TStaticCall of MethodInfo * TExpr list
| TConstructor of Type * TExpr list 
| TAdd of TExpr * TExpr
| TEquals of TExpr * TExpr
| TIf of TExpr * TExpr
| TWhile of TExpr * TExpr
| TDoWhile of TExpr * TExpr 
| TReturn of TExpr
| TAssign of TExpr * TExpr

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

let rec getType = function
| TVar(t,_) -> Some(t)
| TString(_) -> Some(typeof<string>)
| TInt(_) -> Some(typeof<int>)
| TFloat(_) -> Some(typeof<float32>)
| TDouble(_) -> Some(typeof<float>)
| TBool(_) -> Some(typeof<bool>)
| TInstanceCall(_,mi,_)
| TStaticCall(mi,_) -> if mi.ReturnType = typeof<Void> then None else Some(mi.ReturnType)
| TConstructor(t,_) -> Some(t)
| TAdd(e,e') -> getType e //for now assume that you can only add type x to type x
| TEquals(e, e') -> Some(typeof<bool>)
| TReturn(e) -> getType e
| TRef(t, _) -> Some(t)
| TScope(_) -> None
| TWhile(_,_) -> None
| TDoWhile(_,_) -> None
| TAssign(_,_) -> None
| TIf(_, ifTrue) -> getType ifTrue //Note return types from if must match from both sides

//TODO - Lookup locally defined types
let getTypeByName name usings = 
    let t = Type.GetType name
    match t with
    | null -> 
        let usings = usings |>List.rev //We reverse the order as the last defined takes greatest precedence
        let possibleTypeNames = usings |> List.map(fun using -> using + "." + name)
        let loadedAssemblies = AppDomain.CurrentDomain.GetAssemblies()

        let possibleFullyQualifiedTypes = 
            possibleTypeNames 
            |> List.collect (fun t -> loadedAssemblies |> Seq.map(fun a -> t + "," + a.FullName) |> Seq.toList)
            |> List.map(fun tn -> Type.GetType(tn))
            |> List.filter(fun t -> t <> null)
        match possibleFullyQualifiedTypes with
        | head::_ -> head
        | _ -> null
        
    | t -> t
    
let resolveType name usings = 
    match name with
    | "void" -> None
    | "int" -> Some(typeof<int>)
    | "float" -> Some(typeof<float32>)
    | "double" -> Some(typeof<float>)
    | "string" -> Some(typeof<string>)
    | "bool" -> Some(typeof<bool>)
    | "object" -> Some(typeof<obj>)
    | typeName -> 
        let t = getTypeByName typeName usings
        if t <> null then Some(t) 
        else failwith "Unrecognized type"

let rec toTypedExpr usings (variables:Dictionary<_,_>) (parameters:(_ * _) list) = function
     | Expr(expr) -> toTypedExpr usings variables parameters expr
     | Var(typeName, name) -> 
        let t = resolveType typeName usings
        match t with
        | Some(t) -> 
            variables.Add(name, t)
            TVar(t, name)
        | None -> failwith "void not a valid type for a variable" 
     | Ref(name) -> 
        let var = match variables.TryGetValue(name) with
                  | true, value -> Some(value)
                  | _ -> None
        let param = parameters |> List.tryFind(fun (t,n) -> n = name)
        match var, param with
        | Some(v), _ -> TRef(v, name)
        | None, Some(v,n) -> TRef(v, name)
        | _ -> failwith "Unsupported"
     | String(s) -> TString(s)
     | Int(i) -> TInt(i)
     | Float(f) -> TFloat(f)
     | Double(d) -> TDouble(d)
     | Bool(b) -> TBool(b)
     | Call(name, parameters') -> 
        //If theres a . then its either an instance call or a static call
        let firstDot = name.IndexOf('.')
        if firstDot <> -1 then
            //See if the 
            let beforeDot = name.Substring(0, firstDot)
            let var = match variables.TryGetValue beforeDot with
                      | true, t -> 
                            let methodName = name.Substring(name.LastIndexOf(".") + 1 )
                            let parameters' = parameters' |> List.map(fun p -> toTypedExpr usings variables parameters p)

                            let mi = t.GetMethod(methodName, parameters'|>List.map(getType)|> List.choose id |> List.toArray)
                            //Now its not entirely true that this will always be a ref.....
                            TInstanceCall(TRef(t, beforeDot), mi, parameters')

                      | _ ->                     
                            let className = name.Substring(0,name.LastIndexOf("."))
                            let methodName = name.Substring(name.LastIndexOf(".") + 1 )
                            let typeOf = getTypeByName className usings
                            let parameters' = parameters' |> List.map(fun p -> toTypedExpr usings variables parameters p)
                            
                            let mi = typeOf.GetMethod(methodName, parameters'|>List.map(getType)|> List.choose id |> List.toArray)
                            TStaticCall(mi, parameters') 
            var
        else
            failwith "'this' methods not supported yet"
     | Constructor(typeName, parameters') -> 
        let t = resolveType typeName usings
        match t with
        | Some(t) -> TConstructor(t,  parameters' |> List.map(fun p -> toTypedExpr usings variables parameters p)) 
        | None -> failwith "void not a valid type for a variable" 
        
     | Add(expr, expr') -> TAdd(toTypedExpr usings variables parameters expr, toTypedExpr usings variables parameters expr') 
     | Equals(expr, expr') -> TEquals(toTypedExpr usings variables parameters expr, toTypedExpr usings variables parameters expr') 
     | Return(expr) -> TReturn(toTypedExpr usings variables parameters expr)
     | Scope(exprs) -> TScope(exprs|>List.map(fun expr -> toTypedExpr usings variables parameters expr))
     | If(cond, ifTrue) -> TIf(toTypedExpr usings variables parameters cond, toTypedExpr usings variables parameters ifTrue)
     | While(cond, body) -> TWhile(toTypedExpr usings variables parameters cond, toTypedExpr usings variables parameters body)
     | DoWhile(body, cond) -> TDoWhile(toTypedExpr usings variables parameters body, toTypedExpr usings variables parameters cond)
     | Assign(expr, expr') -> 
        TAssign(toTypedExpr usings variables parameters expr, toTypedExpr usings variables parameters expr')

let (|CLASSMETHOD|_|) (b, usings) = 
    match b with
    | ClassBody.Method(acccessModifier, typename, name, parameters, exprList) -> 
        let returnType = resolveType typename usings
        let parameters = parameters |> List.map(fun (Parameter(typename, name)) -> (resolveType typename usings).Value, name) //TODO - dont use .Value
        //MUTABLE STATE!!!
        let variables = Dictionary<_,_>()
        Some(TClassBody.TMethod(acccessModifier, returnType, name, parameters, exprList|>List.map(fun e -> toTypedExpr usings variables parameters e)))
    | _ -> None

let (|CLASS|_|) (mb:ModuleBuilder, body:NamespaceBody, namespaceName, usings) = 
    match body with
    | Class(name, visibility, body) -> 
        let definedType = mb.DefineType(namespaceName+"."+name, accessModifierToTypeAttribute visibility)
        let methods = body |> List.choose(fun b -> match (b, usings) with CLASSMETHOD(m) -> Some(m) | _ -> None)
        Some(definedType, methods)
    | _ -> None

let (|INTERFACEMETHOD|_|) (b, usings) =
    match b with 
    | Method(typename, name, parameters) -> 
        let returnType = resolveType typename usings
        let parameters = parameters|>List.map(fun (Parameter(typename, name)) -> (resolveType typename usings).Value, name) //todo - dont use .Value
        Some(TInterfaceBody.TMethod(returnType, name, parameters))
    | _ -> None

let (|INTERFACE|_|) (mb:ModuleBuilder, body:NamespaceBody, namespaceName, usings) = 
    match body with
    | Interface(name, visibility, body) -> 
        let definedType = mb.DefineType(namespaceName+"."+name, TypeAttributes.Abstract ||| TypeAttributes.Interface ||| accessModifierToTypeAttribute visibility)
        let methods = body |> List.choose(fun b -> match (b, usings) with INTERFACEMETHOD(m) -> Some(m) | _ -> None)
        Some(definedType, methods)
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
    | CLASS(tb) -> Some(TClass(tb))
    | INTERFACE(tb, body) -> Some(TInterface(tb, body)) 
    | ENUM(tb) -> Some(TEnum(tb))
    | _ -> failwith "Unrecognized Namespace Body"

let typed  filename (ast: File) = 
    let aName = AssemblyName(filename)
    let ab = AppDomain.CurrentDomain.DefineDynamicAssembly(aName, AssemblyBuilderAccess.RunAndSave)
    let mb = ab.DefineDynamicModule(aName.Name, aName.Name + ".dll", true) //true means debug it seems
    match ast with
    | File fileBody -> 
        let usings = fileBody |> List.choose(fun x -> match x with Using(using) -> Some(using) | _ -> None)
        let namespaces = fileBody |> List.choose(fun x -> match x with Namespace(name, bodyList) -> Some(name, bodyList) | _ -> None)
        ab, TFile(namespaces
                  |>List.collect(fun (name, body) -> body|>List.choose(fun b -> compileType mb b name usings)))
        