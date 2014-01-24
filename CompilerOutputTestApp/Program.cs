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
        int i = 57;
        public void FizzBuzz(int max)
        {
            for (int i = 0; i < max; i = i + 1)
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
        
        static void Main(string[] args)
        {
            var x = new TestNamespace.TestClass();
            x.Fibonacci(100);
            Console.ReadLine();
            Console.Clear();
            x.FizzBuzz(16);
            Console.ReadLine();
        }
    }
}
