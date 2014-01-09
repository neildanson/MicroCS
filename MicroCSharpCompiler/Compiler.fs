module Compiler

open System
open System.Reflection
open System.Reflection.Emit

open Ast
open TypedAst

//Things to consider:
//EnumBuilder and TypeBuilder both need to CreateType, but method defined seperately

let rec eval (il:ILGenerator) (vars:Map<string,LocalBuilder>)  = function
    | TConstructor(t, parameters) ->
        il.Emit(OpCodes.Newobj, t.GetConstructor([||])) //Todo get params from Parameter Expressions
        vars
    | TInstanceCall(TRef(_,name), methodInfo, parameters) when vars.[name].LocalType.IsValueType ->
        il.Emit(OpCodes.Ldloca, vars.[name]) 
        let vars = parameters|>List.fold(fun v p -> eval il v p) vars 
        il.EmitCall(OpCodes.Call, methodInfo, null) 
        vars
    | TInstanceCall(expr, methodInfo, parameters) ->
        let vars = eval il vars expr
        let vars = parameters|>List.fold(fun v p -> eval il v p) vars 
        il.EmitCall(OpCodes.Call, methodInfo, null)  
        vars
    | TStaticCall(methodInfo, parameters) -> 
        let vars = parameters|>List.fold(fun v p -> eval il v p) vars 
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
        il.Emit(OpCodes.Ldloc, vars.[name])
        vars
    | TVar(t, name) -> 
        let local = il.DeclareLocal(t)
        local.SetLocalSymInfo(name)
        vars|>Map.add name local
    | TAdd(s, s') when (getType s) = Some(typeof<string>) &&
                       (getType s') = Some(typeof<string>)  ->
         let vars = eval il vars s
         let vars = eval il vars s'
         il.Emit(OpCodes.Call, typeof<string>.GetMethod("Concat", [| typeof<string>; typeof<string> |]))
         vars
    | TAdd(expr, expr') -> 
        let vars = eval il vars expr
        let vars = eval il vars expr'
        il.Emit(OpCodes.Add)
        vars
    | TEquals(expr, expr') -> 
        let vars = eval il vars expr
        let vars = eval il vars expr'
        il.Emit(OpCodes.Ceq)
        vars
    | TReturn(expr) ->
        let vars = eval il vars expr
        il.Emit(OpCodes.Ret)
        vars  
    | TScope(exprs) -> 
        il.BeginScope()
        let vars = exprs|>List.fold(fun v p -> eval il v p) vars 
        il.EndScope()
        vars
    | TIf(cond, ifTrue) -> 
        //TODO
        let label = il.DefineLabel()
        let ifLocal = il.DeclareLocal(typeof<bool>)
        let vars = eval il vars cond
        il.Emit(OpCodes.Stloc, ifLocal)
        il.Emit(OpCodes.Ldloc, ifLocal)

        il.Emit(OpCodes.Brfalse, label)
        
        let vars = eval il vars ifTrue
        il.MarkLabel(label)
         
        vars
    | TWhile(cond, body) ->
        let startLabel = il.DefineLabel()
        let endLabel = il.DefineLabel()
        //let ifLocal = il.DeclareLocal(typeof<bool>)
        il.MarkLabel(startLabel)
        let vars = eval il vars cond
        //il.Emit(OpCodes.Stloc, ifLocal)
        //il.Emit(OpCodes.Ldloc, ifLocal)
        //If cond == false goto end       
        il.Emit(OpCodes.Brfalse, endLabel)
        //While body
        let vars = eval il vars body
        il.Emit(OpCodes.Br_S, startLabel)
        il.MarkLabel(endLabel)
        vars
    | TDoWhile(body,cond) ->
        //this is not correct
        let startLabel = il.DefineLabel()
        let ifLocal = il.DeclareLocal(typeof<bool>)
        il.MarkLabel(startLabel)
        let vars = eval il vars body
        
        let vars = eval il vars cond
        il.Emit(OpCodes.Stloc, ifLocal)
        il.Emit(OpCodes.Ldloc, ifLocal)
        //If cond == false goto end       
        //While body
        il.Emit(OpCodes.Brtrue, startLabel)
        vars
    | TAssign(TVar(type', name), rhs) -> 
        //TODO
        let vars = eval il vars (TVar(type',name))
        let local = vars.[name]
        let vars = eval il vars rhs
        il.Emit(OpCodes.Stloc, local)        
        vars
    | TAssign(TRef(type', name), rhs) -> 
        //TODO
        let local = vars.[name]
        let vars = eval il vars rhs
        il.Emit(OpCodes.Stloc, local)        
        vars
    | _ -> failwith "Currently unsupported"

let compileInterface (tb:TypeBuilder) body = 
    body|>List.iter(fun (TInterfaceBody.TMethod(returnType, name, parameters)) -> 
                    ignore<| match returnType with
                             | Some(returnType) -> tb.DefineMethod(name, MethodAttributes.Abstract ||| MethodAttributes.Virtual, returnType, 
                                                                   parameters|>List.map(fun (returnType, name) -> returnType)|>List.toArray) 
                             | None ->  tb.DefineMethod(name, MethodAttributes.Abstract ||| MethodAttributes.Virtual))
    tb

let compileMethod parameters (mb:MethodBuilder) (exprList:TExpr list) = 
    let il = mb.GetILGenerator()
    
    exprList|>List.fold(fun vars expr -> eval il vars expr) Map.empty
    il.Emit(OpCodes.Ret)

let compileClass (tb:TypeBuilder) body = 
    body|>List.iter(fun (TClassBody.TMethod(modifier, returnType, name, parameters, body)) -> ignore <|
                        compileMethod parameters
                            (match returnType with
                             | Some(returnType) -> tb.DefineMethod(name, accessModifierToMethodAttribute modifier, returnType, 
                                                                   parameters|>List.map(fun (returnType, name) -> returnType)|>List.toArray) 
                             | None ->  tb.DefineMethod(name, accessModifierToMethodAttribute modifier))  body)
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

