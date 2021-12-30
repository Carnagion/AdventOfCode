using System;
using System.Collections.Generic;
using System.Text;

namespace Aoc.Extensions
{
    public static class ArrayExtensions
    {
        public static T[] Invert<T>(this T[] array)
        {
            T[] inverted = new T[array.Length];
            for (int index = 0; index < array.Length; index += 1)
            {
                inverted[array.Length - 1 - index] = array[index];
            }
            return inverted;
        }

        public static T[,] Expand<T>(this T[,] array, int multiplier, Func<T, T> selector)
        {
            int originalSize = (int)Math.Sqrt(array.Length);
            int newSize = originalSize * multiplier;

            T[,] expanded = new T[newSize, newSize];
            for (int row = 0; row < originalSize; row += 1)
            {
                for (int column = 0; column < originalSize; column += 1)
                {
                    expanded[column, row] = array[column, row];
                }
            }

            for (int row = 0; row < originalSize; row += 1)
            {
                for (int column = originalSize; column < newSize; column += 1)
                {
                    T original = expanded[column - originalSize, row];
                    T changed = selector(original);
                    expanded[column, row] = changed;
                }
            }

            for (int row = originalSize; row < newSize; row += 1)
            {
                for (int column = 0; column < newSize; column += 1)
                {
                    T original = expanded[column, row - originalSize];
                    T changed = selector(original);
                    expanded[column, row] = changed;
                }
            }
            
            return expanded;
        }

        public static IEnumerable<T> Adjacent<T>(this T[,] array, long row, long column)
        {
            for (long adjacentRow = row - 1; adjacentRow <= (row + 1); adjacentRow += 1)
            {
                for (long adjacentColumn = column - 1; adjacentColumn <= (column + 1); adjacentColumn += 1)
                {
                    yield return array[adjacentColumn, adjacentRow];
                }
            }
        }

        public static List<string> ToList(this char[,] array, long rows, long columns)
        {
            List<string> list = new();
            for (long row = 0; row < rows; row += 1)
            {
                StringBuilder builder = new();
                for (long column = 0; column < columns; column += 1)
                {
                    builder.Append(array[column, row]);
                }
                list.Add(builder.ToString());
            }
            return list;
        }

        public static void Print<T>(this T[,] array, int rows, int columns)
        {
            for (int row = 0; row < rows; row += 1)
            {
                for (int column = 0; column < columns; column += 1)
                {
                    Console.Write(array[column, row]);
                }
                Console.WriteLine();
            }
        }
    }
}