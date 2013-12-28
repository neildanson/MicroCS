﻿module Compiler

open System
open System.Reflection
open System.Reflection.Emit

open Ast
open TypedAst

//Things to consider:
//EnumBuilder and TypeBuilder both need to CreateType, but method defined seperately

let rec eval (il:ILGenerator) (vars:Map<string,LocalBuilder>) (usings:string list) = function
    | TConstructor(t, parameters) ->
        il.Emit(OpCodes.Newobj, t.GetConstructor([||])) //Todo get params from Parameter Expressions
        vars
    | TCall(name, parameters) -> 
        let vars = parameters|>List.fold(fun v p -> eval il v usings p) vars 
        let className = name.Substring(0,name.LastIndexOf("."))
        let methodName = name.Substring(name.LastIndexOf(".") + 1 )
        let typeOf = getTypeByName className usings
        let mi = typeOf.GetMethod(methodName, [|typeof<string>|])
        il.EmitCall(OpCodes.Call, mi, null)  
        vars
    | TString(s) -> il.Emit(OpCodes.Ldstr,s)
                    vars
    | TInt(i) -> il.Emit(OpCodes.Ldc_I4, i)
                 vars
    | TBool(b) -> 
        il.Emit(OpCodes.Ldc_I4, if b then 1 else 0)
        vars
    | TRef(name) -> 
        il.Emit(OpCodes.Ldloc, vars.[name])
        vars
    | TVar(t, name, expr) -> 
        let local = il.DeclareLocal(t)
        local.SetLocalSymInfo(name)
        match expr with 
        | Some(expr) -> let vars = eval il vars usings expr
                        il.Emit(OpCodes.Stloc, local)
                        vars.Add(name, local)
        | _ -> vars
    | TAdd(expr, expr') -> 
        let vars = eval il vars usings expr
        let vars = eval il vars usings expr'
        il.Emit(OpCodes.Add)
        vars
    | TReturn(expr) ->
        let vars = eval il vars usings expr
        il.Emit(OpCodes.Ret)
        vars  
    | _ -> failwith "Currently unsupported"

let compileInterface (tb:TypeBuilder) body usings= 
    body|>List.iter(fun (TInterfaceBody.TMethod(returnType, name, parameters)) -> 
                    ignore<| match returnType with
                             | Some(returnType) -> tb.DefineMethod(name, MethodAttributes.Abstract ||| MethodAttributes.Virtual, returnType, 
                                                                   parameters|>List.map(fun (returnType, name) -> returnType)|>List.toArray) 
                             | None ->  tb.DefineMethod(name, MethodAttributes.Abstract ||| MethodAttributes.Virtual))
    tb

let compileMethod parameters (mb:MethodBuilder) (exprList:TExpr list) usings = 
    let il = mb.GetILGenerator()
    usings|>List.iter (fun using -> il.UsingNamespace using)
    
    exprList|>List.fold(fun vars expr -> eval il vars usings expr) Map.empty
    

let compileClass (tb:TypeBuilder) body usings= 
    body|>List.iter(fun (TClassBody.TMethod(modifier, returnType, name, parameters, body)) -> ignore <|
                        compileMethod parameters
                            (match returnType with
                             | Some(returnType) -> tb.DefineMethod(name, accessModifierToMethodAttribute modifier, returnType, 
                                                                   parameters|>List.map(fun (returnType, name) -> returnType)|>List.toArray) 
                             | None ->  tb.DefineMethod(name, accessModifierToMethodAttribute modifier))  body usings)
    tb

let compileType usings = function
    | TInterface(tb, body) -> Some(compileInterface tb body usings)
    | TClass(tb, body) -> Some(compileClass tb body usings)
    | TStruct(tb) -> Some(tb)
    | TEnum(_) -> None (*enums already compiled*)

let compileFile(TFile(usings, body)) = 
    body
    |>List.choose(fun b -> compileType usings b)
    |>List.iter(fun tb -> tb.CreateType() |> ignore)

let compile (ast: TFile) = 
    compileFile ast

