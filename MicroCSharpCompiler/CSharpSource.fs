module CSharpSource

// An example c# prograsm to identify some of the capabilities of the compiler
// done - while loops, if statements, local variables, instance calls (including this), static calls, basic arithmatic, for loops, read/write fields
// still todo - ifthenelse,  <=, >=, !=, switch, labels, inheritance,  properties

let cSharpProgram = """
using System;
using System.Net;
using System.Threading;
using System.Diagnostics;
namespace TestNamespace
{
    public interface TestInterface
    {
        int DoSomething(int hello, double goodbye);
    }

    public class TestClass
    {
        int field;
        
        public int GetField()
        {
            field = field + 1;
            return field;
        }
        
        public void Fibonacci(int count)
        {
            int n = 0;
            while (n < count)
            {
                int i = 0;
                int a = 0;
                int b = 1;
                while (i < n)
                {
                    int temp = a;
                    a = b;
                    b = temp + a;
                    i = i + 1;
                }
                n = n + 1;
                Console.WriteLine(a.ToString());
            }
        }

        public void FizzBuzz(int max)
        {
            for (int i = 1; i < max; i = i + 1)
            {
                int rem5 = i % 5;
                int rem3 = i % 3;
                if (rem3 == 0)
                    Console.Write("Fizz");
                if (rem5 == 0)
                    Console.Write("Buzz");
                if ((rem5 > 0) && (rem3 > 0))
                    Console.Write(i);
                Console.WriteLine();
            }
        }
    }

    public struct TestStruct
    {
    }

    internal enum TestEnum
    {
        Value1, Value2, Terminator, Terminator2
    }
}
"""

//TODO Arrays, For loops, this method calls, fields, properties, constructors, ||, <=, >=, !=, inheritance