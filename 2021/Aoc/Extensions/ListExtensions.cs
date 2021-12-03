using System;
using System.Collections.Generic;

namespace Aoc.Extensions
{
    public static class ListExtensions
    {
        public static void RemoveAllButOne<T>(this List<T> list, Predicate<T> match)
        {
            int index = 0;
            while ((list.Count > 1) && (index < list.Count))
            {
                if (match(list[index]))
                {
                    list.RemoveAt(index);
                    continue;
                }
                index += 1;
            }
        }
    }
}