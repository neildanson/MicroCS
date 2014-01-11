using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CompilerOutputTestApp
{
    class Program
    {
        static void Main(string[] args)
        {
            var x = new TestNamespace.TestClass();
            var y = x.TestParameterAdd(40, 50);
            var r = x.NonDefaultConstructor();
            x.TestWhile2();
            x.TestWhile();
            x.DoSomething2();
            //x.TestScoping();
            x.TestEquality();
            x.TestInferMethodReturnType();
            x.TestIf();
            //x.TestDoWhile();
            x.Fibonacci(100);
            Console.ReadLine();
        }
    }
}
