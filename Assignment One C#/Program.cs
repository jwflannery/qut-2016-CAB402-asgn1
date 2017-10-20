using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using System.Threading.Tasks;

namespace AssignmentOne
{
    class Program
    {   
        //Create the world, feed it the input, and print the resulting data.
        static void Main(string[] args)
        {   
            Globals.FileName = args.First();
            var inputLines = System.IO.File.ReadLines(args.First());

            var world = new World(inputLines);

            //Print output
            world.PrintAll();

            Console.WriteLine("Complete.");
        }
    }
}


