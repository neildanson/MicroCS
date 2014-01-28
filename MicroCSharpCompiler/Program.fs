open System
open System.Reflection
open System.Diagnostics
open Microsoft.FSharp.Text.Lexing
open Ast
open TypedAst
open Compiler
open Lexer
open Parser

open CSharpSource
open Assembly

let Parse input =
    let tokenized = LexBuffer<char>.FromString(input)
    try
        Parser.start Lexer.tokenize tokenized
    with e ->
        let pos = tokenized.EndPos
        let line = pos.Line
        let column = pos.Column
        let message = e.Message
        let lastToken = new System.String(tokenized.Lexeme)
        printf "Parse failed at line %d, column %d:\n" line column
        printf "Last loken: %s" lastToken
        printf "\n"
        System.Console.ReadLine() |> ignore
        exit 1

let references refs = refs |> List.iter(fun s -> Assembly.LoadWithPartialName s |> ignore)

let Compile() =
    let sw = Stopwatch()
    sw.Restart()

    //Perhaps a new appdomain is in order?
    let ast = Parse cSharpProgram

    "CSharpExample"
    |> CompileFile
    |> CompileTypeStubs ast
    |> CompileMethodStubs
    |> Finish
    |> Save

    printf "Time to Compile %dms" sw.ElapsedMilliseconds
    Console.ReadLine() |> ignore

Compile()