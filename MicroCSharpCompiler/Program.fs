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
using System.Net;
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

        int TestIntAddition()
        {
            return 14 + 3;
        }

        void TestStaticCall()
        {
            Debug.WriteLine("xxx");
        }

        string TestInstanceCall()
        {
            string s = "Hello";
            return s.ToLower();
        }

        string TestComplexCalls()
        {
            WebClient wc = new WebClient();
            
            string result =  wc.DownloadString("http://www.google.com");
            return result;
        }


        void TestInferMethodReturnType()
        {
            string s = Convert.ToString(47);
            Debug.WriteLine(s);
        }

        bool DoSomething2()
        {
            string xxx = "foo" + "bar";
            bool b = true;
            Console.WriteLine(xxx);
            Debug.WriteLine(xxx);
            return b;
        }

        bool TestEquality()
        {
            return 1 == 2;
        }
    }

    internal enum TestEnum 
    {
        Value1, Value2, Terminator, Terminator2
    }
}
"""
let parse input = 
    let tokenized = LexBuffer<char>.FromString(input) 
    Parser.start Lexer.tokenize tokenized            

let save name (ass: Emit.AssemblyBuilder) = ass.Save(name)

let references refs = refs |> List.iter(fun s -> Assembly.LoadWithPartialName s |> ignore)

let Compile() =
    let sw = Stopwatch()
    sw.Restart()

    references [ "System.Xml"] 
    cSharpProgram 
    |> parse 
    |> typed "CSharpCompilerExample"
    |> compile
    |> save "CSharpCompilerExample.dll"
    
    printf "Time to Compile %dms" sw.ElapsedMilliseconds
    Console.ReadLine() |> ignore

IO.File.Delete("CSharpCompilerExample.dll")
Compile()