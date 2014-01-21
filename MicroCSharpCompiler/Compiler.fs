module Compiler

open System
open System.Reflection
open System.Reflection.Emit

open Ast
open TypedAst

//Things to consider:
//EnumBuilder and TypeBuilder both need to CreateType, but method defined seperately

let rec eval (il:ILGenerator) (vars:Map<string,LocalBuilder>) (parameters:ParameterBuilder list) expr =
    let eval expr vars = eval il vars parameters expr 
    match expr with
    | TConstructor(t, ci, parameters') ->
        let vars = parameters'|>List.fold(fun v p -> eval p v) vars
        il.Emit(OpCodes.Newobj, ci) //Todo get params from Parameter Expressions
        vars
    | TInstanceCall(TRef(_,name), methodInfo, parameters') when name = "this" ->
        il.Emit(OpCodes.Ldarg_0)
        //let vars = eval expr vars
        let vars = parameters'|>List.fold(fun v p -> eval p v) vars
        il.EmitCall(OpCodes.Call, methodInfo, null)
        vars
    | TInstanceCall(TRef(_,name), methodInfo, parameters') when vars.[name].LocalType.IsValueType ->
        il.Emit(OpCodes.Ldloca, vars.[name])
        let vars = parameters'|>List.fold(fun v p -> eval p v) vars
        il.EmitCall(OpCodes.Call, methodInfo, null)
        vars
    | TInstanceCall(expr, methodInfo, parameters') ->
        let vars = eval expr vars
        let vars = parameters'|>List.fold(fun v p -> eval p v) vars
        il.EmitCall(OpCodes.Call, methodInfo, null)
        vars
    | TStaticCall(methodInfo, parameters') ->
        let vars = parameters'|>List.fold(fun v p -> eval p v) vars
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
    | TAdd(lhs, rhs) when (getType lhs) = Some(typeof<string>) &&
                          (getType rhs) = Some(typeof<string>)  ->
         let vars = eval lhs vars
         let vars = eval rhs vars
         il.Emit(OpCodes.Call, typeof<string>.GetMethod("Concat", [| typeof<string>; typeof<string> |]))
         vars
    | TAdd(lhs, rhs) ->
        let vars = eval lhs vars
        let vars = eval rhs vars
        il.Emit(OpCodes.Add)
        vars

    | TSubtract(lhs, rhs) ->
        let vars = eval lhs vars
        let vars = eval rhs vars
        il.Emit(OpCodes.Sub)
        vars
    | TMultiply(lhs, rhs) ->
        let vars = eval lhs vars
        let vars = eval rhs vars
        il.Emit(OpCodes.Mul)
        vars
    | TDivide(lhs, rhs) ->
        let vars = eval lhs vars
        let vars = eval rhs vars
        il.Emit(OpCodes.Div)
        vars
    | TModulus(lhs, rhs) ->
        let vars = eval lhs vars
        let vars = eval rhs vars
        il.Emit(OpCodes.Rem)
        vars
    | TEquals(lhs, rhs) ->
        let vars = eval lhs vars
        let vars = eval rhs vars
        il.Emit(OpCodes.Ceq)
        vars
    | TLessThan(lhs, rhs) ->
        let vars = eval lhs vars
        let vars = eval rhs vars
        il.Emit(OpCodes.Clt)
        vars
    | TGreaterThan(lhs, rhs) ->
        let vars = eval lhs vars
        let vars = eval rhs vars
        il.Emit(OpCodes.Cgt)
        vars
    | TAnd(lhs, rhs) ->
        let l1 = il.DefineLabel()
        let l2 = il.DefineLabel()
        let vars = eval lhs vars
        il.Emit(OpCodes.Brfalse, l1)
        let vars = eval rhs vars
        il.Emit(OpCodes.Br_S, l2)
        il.MarkLabel(l1)
        il.Emit(OpCodes.Ldc_I4_0)
        il.MarkLabel(l2)
        vars
    | TReturn(expr) ->
        let vars = eval expr vars
        il.Emit(OpCodes.Ret)
        vars
    | TScope(exprs) ->
        il.BeginScope()
        let vars = exprs|>List.fold(fun v p -> eval p v) vars
        il.EndScope()
        vars
    | TIf(cond, ifTrue) ->
        let label = il.DefineLabel()
        let vars = eval cond vars
        il.Emit(OpCodes.Brfalse, label)
        let vars = eval ifTrue vars
        il.MarkLabel(label)

        vars
    | TWhile(cond, body) ->
        let startLabel = il.DefineLabel()
        let endLabel = il.DefineLabel()
        il.MarkLabel(startLabel)
        let vars = eval cond vars
        //If cond == false goto end
        il.Emit(OpCodes.Brfalse, endLabel)
        //While body
        let vars = eval body vars
        il.Emit(OpCodes.Br_S, startLabel)
        il.MarkLabel(endLabel)
        vars
    | TDoWhile(body,cond) ->
        let startLabel = il.DefineLabel()
        il.MarkLabel(startLabel)
        let vars = eval body vars

        let vars = eval cond vars
        //If cond == false goto end
        //While body
        il.Emit(OpCodes.Brtrue, startLabel)
        vars
    | TAssign(TVar(type', name), rhs) ->
        let vars = eval (TVar(type',name)) vars 
        let local = vars.[name]
        let vars = eval rhs vars
        il.Emit(OpCodes.Stloc, local)
        vars
    | TAssign(TRef(type', name), rhs) ->
        let local = vars.[name]
        let vars = eval rhs vars
        il.Emit(OpCodes.Stloc, local)
        vars
    | x -> failwith "Currently unsupported"

(*
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
    body|>List.iter(fun (TClassBody.TMethod(modifier, returnType, name, parameters, body)) ->
                        ignore <| compileMethod parameters name body)
    tb
        
let compile (ab, ast: TFile) =
    compileFile ast
    ab
    *)