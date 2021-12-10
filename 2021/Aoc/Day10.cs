using System;
using System.Collections.Generic;
using System.Linq;

using Aoc.Extensions;

namespace Aoc
{
    public static class Day10
    {
        public static void Part1()
        {
            List<string> lines = new();
            string input = Console.ReadLine();
            while (input is not null)
            {
                if (input is "done")
                {
                    break;
                }
                lines.Add(input);
                input = Console.ReadLine();
            }

            int syntaxScore = (from line in lines
                               select line.ValidityScore()).Sum();
            Console.WriteLine($"Done. Syntax score: {syntaxScore}");
        }

        public static void Part2()
        {
            List<string> lines = new();
            string input = Console.ReadLine();
            while (input is not null)
            {
                if (input is "done")
                {
                    break;
                }
                lines.Add(input);
                input = Console.ReadLine();
            }

            List<long> scores = new();
            IEnumerable<string> incompleteLines = from line in lines
                                                  where line.ValidityScore() is 0
                                                  select line;
            foreach (string incomplete in incompleteLines)
            {
                string ending = incomplete.MissingEnding();
                long score = 0;
                foreach (char character in ending)
                {
                    score *= 5;
                    score += character.EndingScore();
                }
                scores.Add(score);
            }
            
            scores.Sort();
            Console.WriteLine($"Done. Middle: {scores.Middle()}");
        }
    }
}