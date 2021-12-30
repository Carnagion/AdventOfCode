using System.Collections.Generic;
using System.Linq;

namespace Aoc.Extensions
{
    public static class IEnumerableExtensions
    {
        public static bool ContainsAll<T>(this IEnumerable<T> source, IEnumerable<T> values)
        {
            return values.All(value => source.Contains(value));
        }

        public static bool ContainsAny<T>(this IEnumerable<T> source, IEnumerable<T> values)
        {
            return values.Any(value => source.Contains(value));
        }
    }
}