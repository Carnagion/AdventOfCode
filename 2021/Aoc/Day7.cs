using System;
using System.Collections.Generic;
using System.Linq;

namespace Aoc
{
    public static class Day7
    {
        public static void Part1()
        {
            string input = Console.ReadLine();
            IEnumerable<int> positions = from string value in input!.Split(",")
                                         select Int32.Parse(value);

            int maximum = positions.Max();
            List<int> allcosts = new();
            for (int index = 0; index < maximum; index += 1)
            {
                List<int> costs = new();
                foreach (int position in positions)
                {
                    costs.Add((position > index) ? position - index : index - position);
                }
                allcosts.Add(costs.Sum());
            }
            
            Console.WriteLine($"Done. Minimum cost: {allcosts.Min()}");
        }

        public static void Part2()
        {
            string input = Console.ReadLine();
            IEnumerable<int> positions = from string value in input!.Split(",")
                                         select Int32.Parse(value);

            int maximum = positions.Max();
            List<int> allcosts = new();
            for (int index = 0; index < maximum; index += 1)
            {
                List<int> costs = new();
                foreach (int position in positions)
                {
                    int change = (position > index) ? position - index : index - position;
                    
                    int exponent = 1;
                    int cost = 0;
                    for (int count = 0; count < change; count += 1)
                    {
                        cost += exponent;
                        exponent += 1;
                    }
                    
                    costs.Add(cost);
                }
                allcosts.Add(costs.Sum());
            }
            
            Console.WriteLine($"Done. Minimum cost: {allcosts.Min()}");
        }
    }
}