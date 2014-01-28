module TypedAst

open Ast
open Reflection

open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit

type TExpr =
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
| TConstructor of Type * ConstructorInfo * TExpr list
| TAdd of TExpr * TExpr
| TSubtract of TExpr * TExpr
| TMultiply of TExpr * TExpr
| TDivide of TExpr * TExpr
| TModulus of TExpr * TExpr
| TEquals of TExpr * TExpr
| TLessThan of TExpr * TExpr
| TGreaterThan of TExpr * TExpr
| TAnd of TExpr * TExpr
| TIf of TExpr * TExpr
| TWhile of TExpr * TExpr
| TDoWhile of TExpr * TExpr
| TFor of TExpr * TExpr * TExpr * TExpr
| TReturn of TExpr
| TAssign of TExpr * TExpr

let rec getType = function
| TVar(t,_) -> Some(t)
| TString(_) -> Some(typeof<string>)
| TInt(_) -> Some(typeof<int>)
| TFloat(_) -> Some(typeof<float32>)
| TDouble(_) -> Some(typeof<float>)
| TBool(_) -> Some(typeof<bool>)
| TInstanceCall(_,mi,_)
| TStaticCall(mi,_) -> if mi.ReturnType = typeof<Void> then None else Some(mi.ReturnType)
| TConstructor(t,ci,_) -> Some(t)
| TAdd(e,e') -> getType e //for now assume that you can only add type x to type x
| TSubtract(e,e') -> getType e //for now assume that you can only add type x to type x
| TMultiply(e,e') -> getType e //for now assume that you can only add type x to type x
| TDivide(e,e') -> getType e //for now assume that you can only add type x to type x
| TModulus(e,e') -> Some(typeof<int>) //assume mod only makes sense on ints
| TAnd(_,_)
| TEquals(_, _)
| TLessThan(_,_)
| TGreaterThan(_,_)  -> Some(typeof<bool>)
| TReturn(e) -> getType e
| TRef(t, _) -> Some(t)
| TScope(_) -> None
| TWhile(_,_) -> None
| TDoWhile(_,_) -> None
| TAssign(_,_) -> None
| TIf(_, ifTrue) -> getType ifTrue //Note return types from if must match from both sides
| TFor(_) -> None

let rec toTypedExpr resolveType (variables:Dictionary<_,_>) (parameters:(_ * _) list) (typeBuilder:TypeBuilder) getConstructor getMethod (getField:_*_->FieldInfo) expr=
     let toTyped expr = toTypedExpr resolveType variables parameters typeBuilder getConstructor getMethod getField expr
     match expr with
     | Expr(expr) -> toTyped expr
     | Var(typeName, name) ->
        //TODO Handle Void?
        let t = resolveType typeName
        variables.Add(name, t)
        TVar(t, name)
     | Ref(name) ->
        let var = match variables.TryGetValue(name) with
                  | true, value -> Some(value)
                  | _ -> None
        let param = parameters |> List.tryFind(fun (t,n) -> n = name)
        match var, param with
        | Some(v), _ -> TRef(v, name)
        | None, Some(v,n) -> TRef(v, name)
        | _ -> let field = getField (name ,typeBuilder)
               TRef(field.FieldType,field.Name)
     | String(s) -> TString(s)
     | Int(i) -> TInt(i)
     | Float(f) -> TFloat(f)
     | Double(d) -> TDouble(d)
     | Bool(b) -> TBool(b)
     | Add(lhs, rhs) -> TAdd(toTyped lhs, toTyped rhs)
     | Subtract(lhs, rhs) -> TSubtract(toTyped lhs, toTyped rhs)
     | Multiply(lhs, rhs) -> TMultiply(toTyped lhs, toTyped rhs)
     | Divide(lhs, rhs) -> TDivide(toTyped lhs, toTyped rhs)
     | Equals(lhs, rhs) -> TEquals(toTyped lhs, toTyped rhs)
     | Modulus(lhs, rhs) -> TModulus(toTyped lhs, toTyped rhs)
     | LessThan(lhs, rhs) -> TLessThan(toTyped lhs, toTyped rhs)
     | GreaterThan(lhs,rhs) -> TGreaterThan(toTyped lhs, toTyped rhs)
     | And(lhs,rhs) -> TAnd(toTyped lhs, toTyped rhs)
     | Return(expr) -> TReturn(toTyped expr)
     | Scope(exprs) -> TScope(exprs|>List.map(fun expr -> toTyped expr))
     | If(cond, ifTrue) -> TIf(toTyped cond, toTyped ifTrue)
     | While(cond, body) -> TWhile(toTyped cond, toTyped body)
     | DoWhile(body, cond) -> TDoWhile(toTyped body, toTyped cond)
     | For(expr1, expr2, expr3, body) -> TFor(toTyped expr1, toTyped expr2, toTyped expr3, toTyped body)
     | Assign(lhs, rhs) -> TAssign(toTyped lhs, toTyped rhs)
     //The code below could do with being a little more robust
     | Call(name, parameters') ->
        //If theres a . then its either an instance call or a static call
        let firstDot = name.IndexOf('.')
        if firstDot <> -1 && not (name.StartsWith("this.")) then
            //See if the
            let beforeDot = name.Substring(0, firstDot)
            let var = match variables.TryGetValue beforeDot with
                      | true, t ->
                            let methodName = name.Substring(name.LastIndexOf(".") + 1 )
                            let parameters' = parameters' |> List.map(fun p -> toTyped  p)

                            let mi = t.GetMethod(methodName, parameters'|>List.map(getType)|> List.choose id |> List.toArray)
                            //Now its not entirely true that this will always be a ref.....
                            TInstanceCall(TRef(t, beforeDot), mi, parameters')

                      | _ ->
                            let className = name.Substring(0,name.LastIndexOf("."))
                            let methodName = name.Substring(name.LastIndexOf(".") + 1 )
                            let typeOf = resolveType className 
                            let parameters' = parameters' |> List.map(fun p -> toTyped p)

                            let mi = typeOf.GetMethod(methodName, parameters'|>List.map(getType)|> List.choose id |> List.toArray)
                            TStaticCall(mi, parameters')
            var
        else
            let parameters' = parameters' |> List.map(fun p -> toTyped p)
            let methodName = if name.StartsWith("this.") then name.Remove(0,5) else name
            let mi = getMethod (methodName, typeBuilder, parameters'|>List.map(getType)|> List.choose id |> List.toArray)
            TInstanceCall(TRef(typeBuilder, "this"), mi, parameters')
     | Constructor(typeName, parameters') ->
        let t = resolveType typeName
        let parameters' = parameters' |> List.map(fun p -> toTyped p)
        let ci = getConstructor (t ,parameters'|>List.map(getType)|> List.choose id |> List.toArray)
        TConstructor(t, ci,  parameters')
     