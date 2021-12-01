using System;
using System.Collections.Generic;

namespace Aoc
{
    public static class Day1
    {
        public static void Part1()
        {
            int count = 0;
            string input = Console.ReadLine();
            int measurement = Int32.Parse(input!);
            while (input is not null)
            {
                if (!Int32.TryParse(input, out int integer))
                {
                    Console.WriteLine($"Done. Count: {count}");
                    break;
                }
                
                if (integer > measurement)
                {
                    count += 1;
                }
                measurement = integer;
                
                input = Console.ReadLine();
            }
        }

        public static void Part2()
        {
            List<int> values = new();
            while (true)
            {
                if (!Int32.TryParse(Console.ReadLine(), out int integer))
                {
                    break;
                }
                values.Add(integer);
            }

            int measurement = 0;
            int count = -1;
            while (values.Count >= 3)
            {
                int sum = 0;
                for (int index = 0; index < 3; index += 1)
                {
                    sum += values[index];
                }
                if (sum > measurement)
                {
                    count += 1;
                }
                measurement = sum;
                values.RemoveAt(0);
            }
            Console.WriteLine($"Done. Count: {count}");
        }
    }
}