using System;
using System.Collections.Generic;
using System.Linq;

using Bibliographer.Formats.Mla;

namespace Aoc
{
    public static class Day6
    {
        public static void Part1()
        {
            string input = Console.ReadLine();
            List<int> lanternfish = (from string value in input!.Split(",")
                                     select Int32.Parse(value)).ToList();

            input = Console.ReadLine();
            int days = Int32.Parse(input!);

            while (days > 0)
            {
                int count = lanternfish.Count;
                for (int index = 0; index < count; index += 1)
                {
                    if (lanternfish[index] is 0)
                    {
                        lanternfish[index] = 6;
                        lanternfish.Add(8);
                        continue;
                    }
                    lanternfish[index] -= 1;
                }
                days -= 1;
            }

            Console.WriteLine($"Done. Count: {lanternfish.Count}");
        }

        [Author("DjordjeNedovic")]
        [Title("DjordjeNedovic/Advent-of-Code")]
        [Container("GitHub")]
        [Location("https://github.com/DjordjeNedovic/Advent-of-Code/blob/main/Advent-of-Code-2021/day-6/Program.cs")]
        [Accessed(2021, 12, 6)]
        public static void Part2()
        {
            string input = Console.ReadLine();
            IEnumerable<int> lanternfish = from string value in input!.Split(",")
                                           select Int32.Parse(value);

            input = Console.ReadLine();
            int days = Int32.Parse(input!);

            long[] generations = new long[9];
            foreach (int fish in lanternfish)
            {
                generations[fish] += 1;
            }

            for (int day = 0; day < days; day += 1)
            {
                long newfish = generations[0];
                for (int count = 1; count < generations.Length; count += 1)
                {
                    generations[count - 1] = generations[count];
                }
                generations[8] = newfish;
                generations[6] += newfish;
            }
            
            Console.WriteLine($"Done. Count: {generations.Sum()}");
        }
    }
}