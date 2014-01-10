module Compiler

open System
open System.Reflection
open System.Reflection.Emit

open Ast
open TypedAst

//Things to consider:
//EnumBuilder and TypeBuilder both need to CreateType, but method defined seperately

let rec eval (il:ILGenerator) (vars:Map<string,LocalBuilder>) (parameters:ParameterBuilder list) = function
    | TConstructor(t, ci, parameters') ->
        let vars = parameters'|>List.fold(fun v p -> eval il v parameters p) vars 
        il.Emit(OpCodes.Newobj, ci) //Todo get params from Parameter Expressions
        vars
    | TInstanceCall(TRef(_,name), methodInfo, parameters') when vars.[name].LocalType.IsValueType ->
        il.Emit(OpCodes.Ldloca, vars.[name]) 
        let vars = parameters'|>List.fold(fun v p -> eval il v parameters p) vars 
        il.EmitCall(OpCodes.Call, methodInfo, null) 
        vars
    | TInstanceCall(expr, methodInfo, parameters') ->
        let vars = eval il vars parameters expr
        let vars = parameters'|>List.fold(fun v p -> eval il v parameters p) vars 
        il.EmitCall(OpCodes.Call, methodInfo, null)  
        vars
    | TStaticCall(methodInfo, parameters') -> 
        let vars = parameters'|>List.fold(fun v p -> eval il v parameters p) vars 
        il.EmitCall(OpCodes.Call, methodInfo, null)  
        vars
    | TString(s) -> il.Emit(OpCodes.Ldstr,s)
                    vars
    | TInt(i) -> il.Emit(OpCodes.Ldc_I4, i)
                 vars
    | TBool(b) -> 
        il.Emit(OpCodes.Ldc_I4, if b then 1 else 0)
        vars
    | TRef(_,name) ->
        let var = vars|> Map.tryFind name
        let param = parameters |>List.tryFind(fun p -> p.Name = name)
        match var, param with
        | Some(v), _ -> il.Emit(OpCodes.Ldloc, v)
        | None, Some(p)  -> il.Emit(OpCodes.Ldarg_S, p.Position)
        | None, None -> failwith "Unknown variable"
        vars
    | TVar(t, name) -> 
        let local = il.DeclareLocal(t)
        local.SetLocalSymInfo(name)
        vars|>Map.add name local
    | TAdd(s, s') when (getType s) = Some(typeof<string>) &&
                       (getType s') = Some(typeof<string>)  ->
         let vars = eval il vars parameters s
         let vars = eval il vars parameters s'
         il.Emit(OpCodes.Call, typeof<string>.GetMethod("Concat", [| typeof<string>; typeof<string> |]))
         vars
    | TAdd(expr, expr') -> 
        let vars = eval il vars parameters expr
        let vars = eval il vars parameters expr'
        il.Emit(OpCodes.Add)
        vars
    
    | TSubtract(expr, expr') -> 
        let vars = eval il vars parameters expr
        let vars = eval il vars parameters expr'
        il.Emit(OpCodes.Sub)
        vars
    | TMultiply(expr, expr') -> 
        let vars = eval il vars parameters expr
        let vars = eval il vars parameters expr'
        il.Emit(OpCodes.Mul)
        vars
    | TDivide(expr, expr') -> 
        let vars = eval il vars parameters expr
        let vars = eval il vars parameters expr'
        il.Emit(OpCodes.Div)
        vars
    | TEquals(expr, expr') -> 
        let vars = eval il vars parameters expr
        let vars = eval il vars parameters expr'
        il.Emit(OpCodes.Ceq)
        vars
    | TLessThan(expr, expr') -> 
        let vars = eval il vars parameters expr
        let vars = eval il vars parameters expr'
        il.Emit(OpCodes.Clt)
        vars
    | TGreaterThan(expr, expr') -> 
        let vars = eval il vars parameters expr
        let vars = eval il vars parameters expr'
        il.Emit(OpCodes.Cgt)
        vars
    | TReturn(expr) ->
        let vars = eval il vars parameters expr
        il.Emit(OpCodes.Ret)
        vars  
    | TScope(exprs) -> 
        il.BeginScope()
        let vars = exprs|>List.fold(fun v p -> eval il v parameters p) vars 
        il.EndScope()
        vars
    | TIf(cond, ifTrue) -> 
        let label = il.DefineLabel()
        let vars = eval il vars parameters cond
        il.Emit(OpCodes.Brfalse, label)
        let vars = eval il vars parameters ifTrue
        il.MarkLabel(label)
         
        vars
    | TWhile(cond, body) ->
        let startLabel = il.DefineLabel()
        let endLabel = il.DefineLabel()
        il.MarkLabel(startLabel)
        let vars = eval il vars parameters cond
        //If cond == false goto end       
        il.Emit(OpCodes.Brfalse, endLabel)
        //While body
        let vars = eval il vars parameters body
        il.Emit(OpCodes.Br_S, startLabel)
        il.MarkLabel(endLabel)
        vars
    | TDoWhile(body,cond) ->
        let startLabel = il.DefineLabel()
        il.MarkLabel(startLabel)
        let vars = eval il vars parameters body
        
        let vars = eval il vars parameters cond
        //If cond == false goto end       
        //While body
        il.Emit(OpCodes.Brtrue, startLabel)
        vars
    | TAssign(TVar(type', name), rhs) -> 
        let vars = eval il vars parameters (TVar(type',name))
        let local = vars.[name]
        let vars = eval il vars parameters rhs
        il.Emit(OpCodes.Stloc, local)        
        vars
    | TAssign(TRef(type', name), rhs) -> 
        let local = vars.[name]
        let vars = eval il vars parameters rhs
        il.Emit(OpCodes.Stloc, local)        
        vars
    | x -> failwith "Currently unsupported"

let compileInterface (tb:TypeBuilder) body = 
    body|>List.iter(fun (TInterfaceBody.TMethod(returnType, name, parameters)) -> 
                    ignore<| match returnType with
                             | Some(returnType) -> tb.DefineMethod(name, MethodAttributes.Abstract ||| MethodAttributes.Virtual, returnType, 
                                                                   parameters|>List.map(fun (returnType, name) -> returnType)|>List.toArray) 
                             | None ->  tb.DefineMethod(name, MethodAttributes.Abstract ||| MethodAttributes.Virtual))
    tb

let compileMethod (parameters:(Type * Name) list) (mb:MethodBuilder) (exprList:TExpr list) = 
    let il = mb.GetILGenerator()
    let parameters = parameters |> List.mapi(fun i (t,n) -> mb.DefineParameter(i+1, ParameterAttributes.In, n))
    exprList|>List.fold(fun vars expr -> eval il vars parameters expr) Map.empty |> ignore
    //Woder if I should check if ive already emitted a ret?
    il.Emit(OpCodes.Ret)

let compileClass (tb:TypeBuilder) body = 
    body|>List.iter(fun (TClassBody.TMethod(modifier, returnType, name, parameters, body)) -> ignore <|
                        compileMethod parameters name body)
    tb

let compileType = function
    | TInterface(tb, body) -> Some(compileInterface tb body) 
    | TClass(tb, body) -> Some(compileClass tb body)
    | TStruct(tb) -> Some(tb)
    | TEnum(_) -> None (*enums already compiled*)

let compileFile(TFile(body)) = 
    body
    |>List.choose(fun b -> compileType b)
    |>List.iter(fun tb -> tb.CreateType() |> ignore)

let compile (ab, ast: TFile) = 
    compileFile ast
    ab

