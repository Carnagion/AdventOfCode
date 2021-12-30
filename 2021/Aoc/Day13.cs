using System;
using System.Collections.Generic;
using System.Linq;

using Aoc.Extensions;

namespace Aoc
{
    public static class Day13
    {
        public static void Part1()
        {
            List<(int x, int y)> points = new();

            string input = Console.ReadLine();
            while (input is not null)
            {
                if (input is "done")
                {
                    break;
                }
                string[] coordinates = input.Split(',');
                points.Add((Int32.Parse(coordinates[0]), Int32.Parse(coordinates[1])));
                input = Console.ReadLine();
            }

            int xMax = points.Max(point => point.x) + 1;
            int yMax = points.Max(point => point.y) + 1;

            char[,] grid = new char[xMax, yMax];
            grid.FillGrid(yMax, xMax, points);

            string[] folded = grid.FoldGrid(yMax, xMax, out _, out _, -1, 655).StringifyGridHorizontal(yMax, xMax);
            foreach (string line in folded)
            {
                Console.WriteLine(line);
            }

            long count = folded.Aggregate<string, long>(0, (current, line) => current + line.Count(character => character is '#'));
            Console.WriteLine($"Done. Hash count: {count}");
        }

        public static void Part2()
        {
            List<(int x, int y)> points = new();
            string input = Console.ReadLine();
            while (input is not null)
            {
                if (input is "done")
                {
                    break;
                }
                string[] coordinates = input.Split(',');
                points.Add((Int32.Parse(coordinates[0]), Int32.Parse(coordinates[1])));
                input = Console.ReadLine();
            }

            List<(char axis, int value)> folds = new();
            input = Console.ReadLine();
            while (input is not null)
            {
                if (input is "done")
                {
                    break;
                }
                string[] split = input.Split('=');
                folds.Add((split[0][^1], Int32.Parse(split[1])));
                input = Console.ReadLine();
            }

            int xMax = points.Max(point => point.x) + 1;
            int yMax = points.Max(point => point.y) + 1;

            char[,] grid = new char[xMax, yMax];
            grid.FillGrid(yMax, xMax, points);

            int xMaxNew = xMax;
            int yMaxNew = yMax;
            char[,] folded = grid;
            foreach ((char axis, int value) in folds)
            {
                Console.WriteLine($"{axis}: {value}");
                folded = folded.FoldGrid(yMaxNew, xMaxNew, out yMaxNew, out xMaxNew, axis is 'y' ? value : -1, axis is 'x' ? value : -1);
            }
            folded.WriteGrid(yMaxNew, xMaxNew);
        }

        public static void FillGrid(this char[,] grid, int rows, int columns, List<(int x, int y)> points)
        {
            for (int row = 0; row < rows; row += 1)
            {
                for (int column = 0; column < columns; column += 1)
                {
                    grid[column, row] = points.Any(point => (point.x == column) && (point.y == row)) ? '#' : '.';
                }
            }
        }

        public static char[,] FoldGrid(this char[,] grid, int rows, int columns, out int newRows, out int newColumns, int alongRow = -1, int alongColumn = -1)
        {
            if (alongRow is not -1)
            {
                string[] stringified = grid.StringifyGridHorizontal(rows, columns);
                string[] folded = Day13.MergeArrays(stringified[..(alongRow - 1)], stringified[(alongRow + 1)..].Invert());
                return folded.GridifyArrayHorizontal(out newRows, out newColumns);
            }

            if (alongColumn is not -1)
            {
                string[] stringified = grid.StringifyGridVertical(rows, columns);
                string[] folded = Day13.MergeArrays(stringified[..alongColumn], stringified[(alongColumn + 1)..].Invert());
                return folded.GridifyArrayVertical(out newRows, out newColumns);
            }

            newRows = newColumns = 0;
            return null;
        }

        public static string[] StringifyGridHorizontal(this char[,] grid, int rows, int columns)
        {
            string[] representation = new string[rows];
            for (int row = 0; row < rows; row += 1)
            {
                representation[row] = "";
                for (int column = 0; column < columns; column += 1)
                {
                    representation[row] += grid[column, row];
                }
            }
            return representation;
        }

        public static string[] StringifyGridVertical(this char[,] grid, int rows, int columns)
        {
            string[] representation = new string[columns];
            for (int column = 0; column < columns; column += 1)
            {
                representation[column] = "";
                for (int row = 0; row < rows; row += 1)
                {
                    representation[column] += grid[column, row];
                }
            }
            return representation;
        }

        public static char[,] GridifyArrayHorizontal(this string[] array, out int rows, out int columns)
        {
            rows = array.Length;
            columns = array[0].Length;
            
            char[,] grid = new char[array[0].Length, array.Length];
            for (int row = 0; row < rows; row += 1)
            {
                for (int column = 0; column < columns; column += 1)
                {
                    grid[column, row] = array[row][column];
                }
            }
            return grid;
        }

        public static char[,] GridifyArrayVertical(this string[] array, out int rows, out int columns)
        {
            rows = array[0].Length;
            columns = array.Length;

            char[,] grid = new char[array.Length, array[0].Length];
            for (int column = 0; column < columns; column += 1)
            {
                for (int row = 0; row < rows; row += 1)
                {
                    grid[column, row] = array[column][row];
                }
            }
            return grid;
        }

        public static void WriteGrid(this char[,] grid, int rows, int columns)
        {
            Console.Write('\n');
            for (int row = 0; row < rows; row += 1)
            {
                for (int column = 0; column < columns; column += 1)
                {
                    Console.Write(grid[column, row]);
                }
                Console.Write('\n');
            }
        }

        public static string[] MergeArrays(string[] first, string[] second)
        {
            string[] larger = first.Length < second.Length ? second : first;
            string[] smaller = first.Length < second.Length ? first : second;
                
            string[] merged = new string[larger.Length];
            for (int index = 0; index < larger.Length; index += 1)
            {
                merged[index] = (index >= smaller.Length) ? larger[index] : Day13.MergeStrings(larger[index], smaller[index]);
            }
            return merged;
        }

        public static string MergeStrings(string first, string second)
        {
            string merged = "";
            for (int index = 0; index < first.Length; index += 1)
            {
                merged += first[index] is '#' || second[index] is '#' ? '#' : '.';
            }
            return merged;
        }
    }
}