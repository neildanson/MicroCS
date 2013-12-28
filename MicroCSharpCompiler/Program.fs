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
        object TestReturningNewObject1()
        {
            object o = new object();
            return o;
        }

        object TestReturningNewObject2()
        {
            return new object();
        }

        bool TestReturningTrue()
        {
            return true;
        }

        bool TestReturingFalse()
        {
            return false;
        }

        string TestStringAddition()
        {
            string x = "foo" + "bar";
            return x;
        }

        //This is not yet supported, other than to show that comments work OK
        //string TestStringAddition2(string s1, string s2, string s3)
        //{
        //    return s1+s1+s3;
        //}

        int TestIntAddition()
        {
            return 14 + 3;
        }

        void TestStaticCall()
        {
            Debug.WriteLine("xxx");
        }

        bool DoSomething2()
        {
            string xxx = "foo" + "bar";
            bool b = true;
            Console.WriteLine(xxx);
            Debug.WriteLine(xxx);
            return b;
        }
    }

    internal enum TestEnum 
    {
        Value1, Value2, Terminator, Terminator2
    }
}
"""
let parse input = LexBuffer<char>.FromString(input) 

// This should read something like...
// code |> parse |> typed |> compile |> save
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