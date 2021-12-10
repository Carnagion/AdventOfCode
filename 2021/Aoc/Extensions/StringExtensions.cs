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
    }
}