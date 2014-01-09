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
        public bool DoSomething2(string bar)
        {
            string xxx = "foo" + bar;
            bool b = true;
            Console.WriteLine(xxx);
            Debug.WriteLine(xxx);
            return b;
        }

        static void Main(string[] args)
        {
            var x = new TestNamespace.TestClass();
            x.TestWhile();
            x.DoSomething2();
            //x.TestScoping();
            x.TestEquality();
            x.TestInferMethodReturnType();
            x.TestIf();
            x.TestDoWhile();
        }
    }
}
