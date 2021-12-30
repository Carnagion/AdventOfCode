using System.Collections.Generic;

namespace Aoc.Extensions
{
    public static class Int32Extensions
    {
        public static bool IsBetween(this int number, int min, int max)
        {
            return (number > min) && (number < max);
        }

        public static bool IsBetweenInclusive(this int number, int min, int max)
        {
            return (number >= min) && (number <= max);
        }

        public static bool IsOutside(this int number, int min, int max)
        {
            return !number.IsBetweenInclusive(min, max);
        }

        public static bool IsOutsideInclusive(this int number, int min, int max)
        {
            return !number.IsBetween(min, max);
        }

        public static IEnumerable<int> Range(this int from, int to, bool inclusive = false)
        {
            int current = inclusive ? from : from + 1;
            while (inclusive ? current <= to : current < to)
            {
                yield return current;
                current += 1;
            }
        }
    }
}