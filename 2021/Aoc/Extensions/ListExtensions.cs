using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

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

        public static T Middle<T>(this List<T> list)
        {
            return list.Count % 2 is 0 ? list[(list.Count / 2) - 1] : list[(int)Math.Floor((float)list.Count / 2)];
        }

        public static string AsString(this List<char> list)
        {
            return list.Aggregate("", (current, character) => current + character);
        }

        public static (char[,] array, int rows, int columns) To2dArray(this List<string> list)
        {
            char[,] array = new char[list[0].Length, list.Count];
            int row = 0;
            int column = 0;
            for (row = 0; row < list.Count; row += 1)
            {
                for (column = 0; column < list[row].Length; column += 1)
                {
                    array[column, row] = list[row][column];
                }
            }
            return (array, row, column);
        }

        public static void Pad(this List<string> list, char padding, int amount)
        {
            StringBuilder builder = new();
            for (int count = 0; count < amount; count += 1)
            {
                builder.Append(padding);
            }
            string pad = builder.ToString();
            
            for (int index = 0; index < list.Count; index += 1)
            {
                list[index] = $"{pad}{list[index]}{pad}";
            }

            builder.Clear();
            for (int count = 0; count < list[0].Length; count += 1)
            {
                builder.Append(padding);
            }
            string empty = builder.ToString();

            for (int count = 0; count < amount; count += 1)
            {
                list.Insert(0, empty);
                list.Add(empty);
            }
        }
    }
}