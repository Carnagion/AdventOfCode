using System;
using System.Reflection;

namespace Aoc
{
    public static class Program
    {
        public static void Main(string[] args)
        {
            Console.WriteLine("Enter the day:");
            int day = Int32.Parse(Console.ReadLine()!);
            Console.WriteLine("Enter the part:");
            int part = Int32.Parse(Console.ReadLine()!);
            
            Type dayType = typeof(Program).Assembly.GetType($"Aoc.Day{day}");
            MethodInfo partMethod = dayType!.GetMethod($"Part{part}");
            partMethod!.Invoke(null, null);
        }
    }
}