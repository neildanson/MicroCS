// This project type requires the F# PowerPack at http://fsharppowerpack.codeplex.com/releases
// Learn more about F# at http://fsharp.net
// Original project template by Jomo Fisher based on work of Brian McNamara, Don Syme and Matt Valerio
// This posting is provided "AS IS" with no warranties, and confers no rights.

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

let parse input =
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

let save name (ass: Emit.AssemblyBuilder) = ass.Save(name)

let references refs = refs |> List.iter(fun s -> Assembly.LoadWithPartialName s |> ignore)

let Compile() =
    let sw = Stopwatch()
    sw.Restart()

    //Perhaps a new appdomain is in order?
    //references [ "System.Xml"; "System.Core"]
    //cSharpProgram
    //|> parse
    //|> typed "CSharpCompilerExample"
    //|> compile
    //|> save "CSharpCompilerExample.dll"
    
    let ast = parse cSharpProgram

    "CSharpExample.dll"
    |> CompileFile
    |> CompileBody ast
    |> Save

    printf "Time to Compile %dms" sw.ElapsedMilliseconds
    Console.ReadLine() |> ignore

IO.File.Delete("CSharpCompilerExample.dll")
Compile()