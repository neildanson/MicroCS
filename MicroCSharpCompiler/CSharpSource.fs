module CSharpSource
let cSharpProgram = """using System; 
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
        public int TestParameterAdd(int a, int b)
        {
            return a + b;
        }

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

        public void TestInferMethodReturnType()
        {
            string s = Convert.ToString(47);
            Debug.WriteLine(s);
        }

        public void TestWhile()
        {
            int i = 0;
            while ( i < 100)
            {
                i = i + 1;
                Console.WriteLine(i.ToString());
            }
        }

        public void TestWhile2()
        {
            int i = 100;
            while ( i > 0)
            {
                i = i - 1;
                Console.WriteLine(i.ToString());
            }
        }

        public Random NonDefaultConstructor()
        {
            Random r = new Random(100);
            return r;
        }

        public bool DoSomething2()
        {
            string xxx = "foo" + "bar";
            int x = 1;
            int y = 2;
            string yyy = x.ToString() + y.ToString();
            bool b = true;
            Console.WriteLine(xxx);
            Debug.WriteLine(yyy);
            return b;
        }

        public bool TestEquality()
        {
            return 1 == 2;
        }

        public void TestScoping()
        {
            int i = 48;
            {
                int j = 64;
                Console.WriteLine(j.ToString());
            }
            Console.WriteLine(i.ToString());
        }

        public bool TestIf()
        {
            Random r = new Random();
            int next = r.Next();
            if (next == 4) return true;
            return false;
        }

        public void TestDoWhile()
        {
            int i = 50;
            do
            {
                Console.WriteLine("Foo");
                Thread.Sleep(100);
                i = i - 1;
            }
            while (i > 0);
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
                Console.WriteLine(a.ToString())
            }
        }

        public void TestModulus()
        {
            int x = 3 % 1;
        }

        public bool And(bool t, bool f)
        {
            return t && f;
        }

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
                if ((rem5 > 0) && (rem3 > 0))
                    Console.Write(i);
                Console.WriteLine();
                i = i + 1;
            }
        }
    }

    internal enum TestEnum 
    {
        Value1, Value2, Terminator, Terminator2
    }
}
"""
