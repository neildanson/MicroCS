module Compiler

open System
open System.Reflection
open System.Reflection.Emit

open Ast
open TypedAst

let rec eval (il:ILGenerator) (vars:Map<string,LocalBuilder>) (parameters:ParameterBuilder list) (fields:FieldInfo list) expr =
    let eval expr vars = eval il vars parameters fields expr 
    match expr with
    | TConstructor(t, ci, parameters') ->
        let vars = parameters'|>List.fold(fun v p -> eval p v) vars
        il.Emit(OpCodes.Newobj, ci) //Todo get params from Parameter Expressions
        vars
    | TInstanceCall(TRef(_,name), methodInfo, parameters') when name = "this" ->
        il.Emit(OpCodes.Ldarg_0)
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
        let field = fields  |>List.tryFind(fun f -> f.Name = name)
        match var, param, field with
        | Some(v), _, _ -> il.Emit(OpCodes.Ldloc, v)
        | None, Some(p), _  -> il.Emit(OpCodes.Ldarg_S, p.Position)
        | None, None, Some(f) -> 
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldfld, f)
        | None, None, None -> failwith "Unknown variable"
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
    | TFor(expr1, expr2, expr3, body) ->
        let vars = eval expr1 vars 
        let startLabel = il.DefineLabel()
        il.MarkLabel(startLabel)
        let vars = eval expr2 vars
        let vars = eval body vars
        let vars = eval expr3 vars
        il.Emit(OpCodes.Brtrue, startLabel)
        vars
    | TAssign(TVar(type', name), rhs) ->
        let vars = eval (TVar(type',name)) vars 
        let local = vars.[name]
        let vars = eval rhs vars
        il.Emit(OpCodes.Stloc, local)
        vars
    | TAssign(TRef(type', name), rhs) ->
        let local = vars.TryFind name
        let field = fields  |>List.tryFind(fun f -> f.Name = name)
        
        match local, field with
        | Some(local),_ ->
            let vars = eval rhs vars
            il.Emit(OpCodes.Stloc, local)
            vars
        | None, Some(field) -> 
            il.Emit(OpCodes.Ldarg_0)
            let vars = eval rhs vars
            il.Emit(OpCodes.Stfld, field)
            vars
        | _ -> failwith "No local or field found"
    | x -> failwith "Currently unsupported"