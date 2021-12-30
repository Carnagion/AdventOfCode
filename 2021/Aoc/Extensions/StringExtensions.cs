using System;
using System.Collections.Generic;
using System.Linq;

namespace Aoc.Extensions
{
    public static class StringExtensions
    {
        public static int BitCommonality(this string expression)
        {
            int commonality = 0;
            foreach (char bit in expression)
            {
                switch (bit)
                {
                    case '0':
                        commonality += 1;
                        break;
                    case '1':
                        commonality -= 1;
                        break;
                }
            }
            return commonality;
        }

        public static int ValidityScore(this string line)
        {
            Stack<char> brackets = new();
            foreach (char character in line)
            {
                if (brackets.Count is 0)
                {
                    brackets.Push(character);
                    continue;
                }
                
                if (brackets.Peek().IsSameSideBracketAs(character))
                {
                    brackets.Push(character);
                    continue;
                }
                if (brackets.Peek() == character.OppositeChar())
                {
                    brackets.Pop();
                    continue;
                }
                return character.ValidityScore();
            }
            return 0;
        }

        public static string MissingEnding(this string line)
        {
            Stack<char> brackets = new();
            foreach (char character in line)
            {
                if (brackets.Count is 0)
                {
                    brackets.Push(character);
                    continue;
                }
                
                if (brackets.Peek().IsSameSideBracketAs(character))
                {
                    brackets.Push(character);
                    continue;
                }
                if (brackets.Peek() == character.OppositeChar())
                {
                    brackets.Pop();
                    continue;
                }
                break;
            }
            
            string ending = brackets.Aggregate("", (current, character) => current + character.OppositeChar());
            return ending;
        }

        public static char MostCommonChar(this string expression)
        {
            Dictionary<char, long> commonalities = new();
            foreach (char character in expression)
            {
                if (!commonalities.ContainsKey(character))
                {
                    commonalities.Add(character, 0);
                }
                commonalities[character] += 1;
            }
            char mostCommon = commonalities.First().Key;
            foreach ((char key, long _) in commonalities)
            {
                if (commonalities[key] > commonalities[mostCommon])
                {
                    mostCommon = key;
                }
            }
            return mostCommon;
        }

        public static IEnumerable<string> CharacterPairs(this string expression)
        {
            for (int index = 0; index < (expression.Length - 1); index += 1)
            {
                yield return $"{expression[index]}{expression[index + 1]}";
            }
        }
        
        public static char MostCommonChar(this string expression, out long occurrences)
        {
            Dictionary<char, long> commonalities = new();
            foreach (char character in expression)
            {
                if (!commonalities.ContainsKey(character))
                {
                    commonalities.Add(character, 0);
                }
                commonalities[character] += 1;
            }
            char mostCommon = commonalities.First().Key;
            occurrences = commonalities.First().Value;
            foreach ((char key, long value) in commonalities)
            {
                if (commonalities[key] > commonalities[mostCommon])
                {
                    mostCommon = key;
                    occurrences = value;
                }
            }
            return mostCommon;
        }

        public static char LeastCommonChar(this string expression)
        {
            Dictionary<char, long> commonalities = new();
            foreach (char character in expression)
            {
                if (!commonalities.ContainsKey(character))
                {
                    commonalities.Add(character, 0);
                }
                commonalities[character] += 1;
            }
            char leastCommon = commonalities.First().Key;
            foreach ((char key, long _) in commonalities)
            {
                if (commonalities[key] < commonalities[leastCommon])
                {
                    leastCommon = key;
                }
            }
            return leastCommon;
        }
        
        public static char LeastCommonChar(this string expression, out long occurrences)
        {
            Dictionary<char, long> commonalities = new();
            foreach (char character in expression)
            {
                if (!commonalities.ContainsKey(character))
                {
                    commonalities.Add(character, 0);
                }
                commonalities[character] += 1;
            }
            char leastCommon = commonalities.First().Key;
            occurrences = commonalities.First().Value;
            foreach ((char key, long value) in commonalities)
            {
                if (commonalities[key] < commonalities[leastCommon])
                {
                    leastCommon = key;
                    occurrences = value;
                }
            }
            return leastCommon;
        }

        public static string ToBinaryString(this string hex)
        {
            return String.Join(String.Empty, from character in hex
                                             select Convert.ToString(Convert.ToInt32($"{character}", 16), 2).PadLeft(4, '0'));
        }
    }
}