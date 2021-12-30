using System.Collections.Generic;

namespace Aoc.Extensions
{
    public static class GenericExtensions
    {
        public static IEnumerable<T> ToEnumerable<T>(this T t)
        {
            yield return t;
        }
    }
}