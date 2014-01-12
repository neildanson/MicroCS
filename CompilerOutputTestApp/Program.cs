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
        public void FizzBuzz(int max)
        {
            int i = 1;
            while (i < max)
            {
                int rem5 = i % 5;
                int rem3 = i % 3;
                if (rem3 == 0)
                    Console.Write("Fizz");
                if (rem5 == 0)
                    Console.Write("Buzz");
                if (rem5 > 0 && rem3 > 0)
                    Console.Write(i);
                Console.WriteLine();
                i = i + 1;
            }
        }
        static void Main(string[] args)
        {
            var xxx = 30;
            var yyy = xxx % 2;
            var x = new TestNamespace.TestClass();
            var y = x.TestParameterAdd(xxx, 50);
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
            Console.Clear();
            x.FizzBuzz(16);
            Console.ReadLine();
        }
    }
}
