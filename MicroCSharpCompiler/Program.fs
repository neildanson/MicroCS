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

let cSharpProgram = """
using System; 
using System.Diagnostics;
namespace TestNamespace
{
    public interface TestInterface
    {
        int DoSomething(int hello, double goodbye);
    }

    class TestClass
    {
        object DoSomething()
        {
            string xxx = "foo" + "bar";
            object o = new object();
            Console.WriteLine(xxx);
            Debug.WriteLine(xxx);
            return o;
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
    let xmlAssembly = Assembly.LoadWithPartialName "System.Xml"
    let tokenized = parse cSharpProgram
    let sw = Stopwatch()
    sw.Restart()
    printfn "Parsing..."
    let ast = Parser.start Lexer.tokenize tokenized
    printfn "Compiling..."
    let ab, pcAst = preCompile ast "CSharpCompilerExample"
    compile pcAst
    printf "Time to Compile %dms" sw.ElapsedMilliseconds
    ab.Save("CSharpCompilerExample.dll")
    Console.ReadLine() |> ignore

IO.File.Delete("CSharpCompilerExample.dll")
Compile()