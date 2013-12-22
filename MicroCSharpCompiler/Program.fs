// This project type requires the F# PowerPack at http://fsharppowerpack.codeplex.com/releases
// Learn more about F# at http://fsharp.net
// Original project template by Jomo Fisher based on work of Brian McNamara, Don Syme and Matt Valerio
// This posting is provided "AS IS" with no warranties, and confers no rights.

open System
open Microsoft.FSharp.Text.Lexing
open Ast
open PreCompileAst
open Compiler
open Lexer
open Parser

let cSharpProgram = """
using System; 
using System.Net;
namespace TestNamespace
{
    public interface TestInterface
    {
        int DoSomething(int hello, double goodbye);
    }

    class TestClass
    {
        void DoSomething()
        {
            System.Diagnostics.Debug.WriteLine("Woohoo");
        }
    }

    internal enum TestEnum 
    {
        Value1, Value2, Terminator, Terminator2
    }
}
"""
let parse input = LexBuffer<char>.FromString(input) 

let Compile() =
    let tokenized = parse cSharpProgram
    
    printfn "Parsing..."
    let ast = Parser.start Lexer.tokenize tokenized
    printfn "Compiling..."
    let ab, pcAst = preCompile ast "CSharpCompilerExample"
    compile pcAst
    ab.Save("CSharpCompilerExample.dll")
  
Compile()