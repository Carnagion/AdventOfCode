using System;
using System.Collections.Generic;
using System.Linq;

using Bibliographer.Formats.Mla;

namespace Aoc
{
    public static class Day8
    {
        public static void Part1()
        {
            int count = 0;
            
            string input = Console.ReadLine();
            while (input is not null)
            {
                if (input is "done")
                {
                    break;
                }

                string[] output = input.Split(" | ")[1].Split(" ");
                count += (from value in output
                          where value.Length is 2 or 3 or 4 or 7
                          select value).Count();

                input = Console.ReadLine();
            }
            
            Console.WriteLine($"Done. Count: {count}");
        }

        [Author("Bpendragon")]
        [Title("Bpendragon/AdventOfCodeCSharp")]
        [Container("GitHub")]
        [Location("https://github.com/Bpendragon/AdventOfCodeCSharp/blob/b1ffd/AdventOfCode/Solutions/Year2021/Day08-Solution.cs")]
        [Accessed(2021, 12, 8)]
        public static void Part2()
        {
            long sum = 0;
            
            string input = Console.ReadLine();
            while (input is not null)
            {
                if (input is "done")
                {
                    break;
                }

                Dictionary<int, List<char>> wiremap = new();

                List<List<char>> wires = new();
                string[] signals = input.Split(new[] {' ', '|',}, StringSplitOptions.RemoveEmptyEntries);
                foreach (string signal in signals[..10])
                {
                    List<char> sorted = signal.ToList();
                    sorted.Sort();
                    wires.Add(sorted);
                }

                List<string> outputs = new();
                foreach (string signal in signals[10..])
                {
                    List<char> sorted = signal.ToList();
                    sorted.Sort();
                    outputs.Add(new(sorted.ToArray()));
                }
                
                wiremap.Add(1, wires.Find(wire => wire.Count is 2));
                wiremap.Add(4, wires.Find(wire => wire.Count is 4));
                wiremap.Add(7, wires.Find(wire => wire.Count is 3));
                wiremap.Add(8, wires.Find(wire => wire.Count is 7));
                
                wiremap.Add(6, wires.Find(wire => wire.Count is 6 && !wiremap.ContainsValue(wire) && !wiremap[7].All(seven => wire.Contains(seven))));
                
                char c = (from character in wiremap[1]
                          where !wiremap[6].Contains(character)
                          select character).First();
                char f = (from character in wiremap[1]
                          where !new[] {c,}.Contains(character)
                          select character).First();
                wiremap.Add(2, wires.Find(wire => wire.Count is 5 && !wiremap.ContainsValue(wire) && wire.Contains(c) && !wire.Contains(f)));
                wiremap.Add(3, wires.Find(wire => wire.Count is 5 && !wiremap.ContainsValue(wire) && wire.Contains(c) && wire.Contains(f)));
                wiremap.Add(5, wires.Find(wire => wire.Count is 5 && !wiremap.ContainsValue(wire) && !wire.Contains(c) && wire.Contains(f)));

                char e = (from character in wiremap[6]
                          where !wiremap[5].Contains(character)
                          select character).First();
                wiremap.Add(0, wires.Find(wire => wire.Count is 6 && !wiremap.ContainsValue(wire) && wire.Contains(e)));
                wiremap.Add(9, wires.Find(wire => !wiremap.ContainsValue(wire)));

                Dictionary<string, int> reverse = wiremap.ToDictionary(entry => new string(entry.Value.ToArray()), entry => entry.Key);
                string output = "";
                foreach (string value in outputs)
                {
                    output += reverse[value];
                }
                sum += Int32.Parse(output);

                input = Console.ReadLine();
            }
            
            Console.WriteLine($"Done. Sum: {sum}");
        }
    }
}