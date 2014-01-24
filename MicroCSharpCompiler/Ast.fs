namespace Ast
open System

type Name = string
type TypeName = string
type AccessModifier = Public | Private | Internal | Protected

type File =
| File of FileBody list
and FileBody =
| Using of string
| Namespace of Name * NamespaceBody list
and NamespaceBody =
| Interface of Name * AccessModifier * InterfaceBody list
| Class of Name * AccessModifier * ClassBody list
| Struct of Name * AccessModifier
| Enum of Name * AccessModifier * Name list //Should this be Name*int for custom int values?
and ClassBody =
| Field of AccessModifier * TypeName * Name
| Property of Name
| Event of Name
| Method of AccessModifier * TypeName * Name * Parameter list * Expr list
and InterfaceBody =
| Method of TypeName * Name * Parameter list
//| Property, Event, yada yada
and Parameter =
| Parameter of TypeName * Name
and Expr =
| Expr of Expr
| Scope of Expr list
| Var of TypeName * Name
| Ref of Name
| String of string
| Int of int
| Float of float32
| Double of float
| Bool of bool
| Call of Name * Expr list
| Constructor of TypeName * Expr list
| Add of Expr * Expr
| Subtract of Expr * Expr
| Multiply of Expr * Expr
| Divide of Expr * Expr
| Modulus of Expr * Expr
| Equals of Expr * Expr
| LessThan of Expr * Expr
| GreaterThan of Expr * Expr
| And of Expr * Expr
| If of Expr * Expr
| Return of Expr
| While of Expr * Expr
| For of Expr * Expr * Expr * Expr
| DoWhile of Expr * Expr
| Assign of Expr * Expr
module AstHelpers =
    let toAccessModifier = function
    | "public" -> Public
    | ""
    | "private" -> Private
    | "internal" -> Internal
    | "protected" -> Protected
    | _ -> failwith "Unidentified Visibility"