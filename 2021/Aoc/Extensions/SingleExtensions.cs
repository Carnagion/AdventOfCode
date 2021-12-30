using System;

namespace Aoc.Extensions
{
    public static class SingleExtensions
    {
        public static bool IsInteger(this float number)
        {
            return Math.Abs(number - Math.Round(number)) < 0.0001;
        }
        
        public static bool IsBetween(this float number, float min, float max)
        {
            return (number > min) && (number < max);
        }

        public static bool IsBetweenInclusive(this float number, float min, float max)
        {
            return (number >= min) && (number <= max);
        }

        public static bool IsOutside(this float number, float min, float max)
        {
            return !number.IsBetweenInclusive(min, max);
        }

        public static bool IsOutsideInclusive(this float number, float min, float max)
        {
            return !number.IsBetween(min, max);
        }
    }
}