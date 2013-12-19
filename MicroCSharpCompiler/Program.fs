// This project type requires the F# PowerPack at http://fsharppowerpack.codeplex.com/releases
// Learn more about F# at http://fsharp.net
// Original project template by Jomo Fisher based on work of Brian McNamara, Don Syme and Matt Valerio
// This posting is provided "AS IS" with no warranties, and confers no rights.

open System
open Microsoft.FSharp.Text.Lexing
open Ast
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
        int DoSomething();
    }

    class TestClass
    {
    }

    internal enum TestEnum 
    {
        Value1, Value2, Terminator
    }
}
"""

let Compile() =
    let lexbuff = LexBuffer<char>.FromString(cSharpProgram)
    
    printfn "Parsing..."
    let ast = Parser.start Lexer.tokenize lexbuff
    printfn "Compiling..."
    compile ast       
  
Compile()