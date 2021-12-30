using System;
using System.Collections.Generic;
using System.Linq;

using Aoc.Extensions;

using Bibliographer.Formats.Mla;

namespace Aoc
{
    public static class Day14
    {
        public static void Part1()
        {
            string polymer = Console.ReadLine();

            Dictionary<string, char> insertions = new();
            while (true)
            {
                string input = Console.ReadLine();
                if (input is "done")
                {
                    break;
                }

                string[] split = input!.Split(" -> ");
                insertions.Add(split[0], split[1][0]);
            }

            for (int step = 0; step < 10; step += 1)
            {
                List<char> list = polymer!.ToList();
                for (int index = 0; index < (list.Count - 1); index += 2)
                {
                    string pair = $"{list[index]}{list[index + 1]}";
                    if (insertions.ContainsKey(pair))
                    {
                        list.Insert(index + 1, insertions[pair]);
                    }
                }
                polymer = list.AsString();
            }

            char mostCommon = polymer.MostCommonChar(out long mostCount);
            char leastCommon = polymer.LeastCommonChar(out long leastCount);
            Console.WriteLine($"Done. Most common: {mostCommon} ({mostCount}). Least common: {leastCommon} ({leastCount})");
        }

        [Author("SimonBaars")]
        [Title("SimonBaars/AdventOfCode-java")]
        [Container("GitHub")]
        [Location("https://github.com/SimonBaars/AdventOfCode-Java/blob/master/src/main/java/com/sbaars/adventofcode/year21/days/Day14.java")]
        [Accessed(2021, 12, 14)]
        public static void Part2()
        {
            string polymer = Console.ReadLine();

            Dictionary<string, char> insertions = new();
            while (true)
            {
                string input = Console.ReadLine();
                if (input is "done")
                {
                    break;
                }

                string[] split = input!.Split(" -> ");
                insertions.Add(split[0], split[1][0]);
            }

            Dictionary<string, long> pairs = new();
            foreach (string pair in polymer.CharacterPairs())
            {
                if (!pairs.ContainsKey(pair))
                {
                    pairs.Add(pair, 0);
                }
                pairs[pair] += 1;
            }

            Dictionary<char, long> characterCounts = new();
            characterCounts.UpdateCharCounts(polymer);
            for (int step = 0; step < 40; step += 1)
            {
                Dictionary<string, long> updatedPairs = new();
                foreach (string pair in pairs.Keys)
                {
                    long count = pairs[pair];
                    if (!insertions.ContainsKey(pair))
                    {
                        if (!updatedPairs.ContainsKey(pair))
                        {
                            updatedPairs.Add(pair, 0);
                        }
                        updatedPairs[pair] += count;
                        continue;
                    }
                    char insertion = insertions[pair];
                    string left = $"{pair[0]}{insertion}";
                    string right = $"{insertion}{pair[1]}";
                    
                    if (!updatedPairs.ContainsKey(left))
                    {
                        updatedPairs.Add(left, 0);
                    }
                    updatedPairs[left] += count;
                    
                    if (!updatedPairs.ContainsKey(right))
                    {
                        updatedPairs.Add(right, 0);
                    }
                    updatedPairs[right] += count;
                    
                    if (!characterCounts.ContainsKey(insertion))
                    {
                        characterCounts.Add(insertion, 0);
                    }
                    characterCounts[insertion] += count;
                }
                pairs = updatedPairs;
            }

            foreach ((string key, long count) in pairs)
            {
                Console.WriteLine($"{key}: {count}");
            }
            long max = characterCounts.Values.Max();
            long min = characterCounts.Values.Min();
            Console.WriteLine(max - min);
        }

        public static void UpdateCharCounts(this Dictionary<char, long> characterCounts, string expression)
        {
            foreach (char character in expression)
            {
                if (!characterCounts.ContainsKey(character))
                {
                    characterCounts.Add(character, 0);
                }
                characterCounts[character] += 1;
            }
        }

        public static IEnumerable<string> PairInsert(this string expression, Dictionary<string, char> insertions)
        {
            for (int index = 0; index < (expression.Length - 1); index += 1)
            {
                string pair = $"{expression[index]}{expression[index + 1]}";
                yield return $"{expression[index]}{insertions[pair]}{expression[index + 1]}";
            }
        }
    }
}