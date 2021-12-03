using System;
using System.Collections.Generic;
using System.Linq;

using Aoc.Extensions;

namespace Aoc
{
    public static class Day3
    {
        public static void Part1()
        {
            List<string> binaries = new();
            
            string input = Console.ReadLine();
            while (input is not null)
            {
                if (input is "done")
                {
                    break;
                }
                binaries.Add(input);
                input = Console.ReadLine();
            }

            string gamma = "";
            for (int index = 0; index < binaries[0].Length; index += 1)
            {
                string binary = new((from string number in binaries
                                     select number[index]).ToArray());
                char mostCommon = (from char bit in binary
                                   group bit by bit into grouped
                                   orderby grouped.Count() descending
                                   select grouped.Key).First();
                gamma += mostCommon;
            }
            string epsilon = new((from char bit in gamma
                                  select (bit is '0') ? '1' : '0').ToArray());
            
            Console.WriteLine($"Done. Gamma: {Convert.ToInt64(gamma, 2)}. Epsilon: {Convert.ToInt64(epsilon, 2)}");
        }

        public static void Part2()
        {
            List<string> binaries = new();
            string input = Console.ReadLine();
            while (input is not null)
            {
                if (input is "done")
                {
                    break;
                }
                binaries.Add(input);
                input = Console.ReadLine();
            }

            List<string> generators = new(binaries);
            for (int bitposition = 0; bitposition < binaries[0].Length; bitposition += 1)
            {
                string binary = new((from string value in generators
                                     select value[bitposition]).ToArray());
                int commonality = binary.BitCommonality();
                char mostCommon = (commonality <= 0) ? '1' : '0';
                
                generators.RemoveAllButOne(value => value[bitposition] != mostCommon);
                if (generators.Count is 1)
                {
                    break;
                }
            }
            
            List<string> scrubbers = new(binaries);
            for (int bitposition = 0; bitposition < binaries[0].Length; bitposition += 1)
            {
                string binary = new((from string value in scrubbers
                                     select value[bitposition]).ToArray());
                int commonality = binary.BitCommonality();
                char mostCommon = (commonality <= 0) ? '0' : '1';
                
                scrubbers.RemoveAllButOne(value => value[bitposition] != mostCommon);
                if (scrubbers.Count is 1)
                {
                    break;
                }
            }
            
            Console.WriteLine($"Done. Generator: {Convert.ToInt64(generators[0], 2)}. Scrubber: {Convert.ToInt64(scrubbers[0], 2)}");
        }
    }
}