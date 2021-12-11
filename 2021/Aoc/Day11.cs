using System;
using System.Collections.Generic;
using System.Linq;

namespace Aoc
{
    public static class Day11
    {
        public static void Part1()
        {
            int[,] octopuses = new int[10, 10];

            int row = 0;
            int column;
            string input = Console.ReadLine();
            while (input is not null)
            {
                if (input is "done")
                {
                    break;
                }
                column = 0;
                foreach (int number in from character in input
                                       select Int32.Parse($"{character}"))
                {
                    octopuses[column, row] = number;
                    column += 1;
                }
                row += 1;
                input = Console.ReadLine();
            }

            long flashes = 0;
            int side = (int)Math.Sqrt(octopuses.Length);
            for (int step = 1; step <= 100; step += 1)
            {
                List<(int flashedRow, int flashedColumn)> flashed = new();
                for (row = 0; row < side; row += 1)
                {
                    for (column = 0; column < side; column += 1)
                    {
                        if (flashed.Contains((row, column)))
                        {
                            continue;
                        }
                        octopuses[column, row] += 1;
                        if (octopuses[column, row] > 9)
                        {
                            octopuses.PerformFlash(row, column, flashed);
                        }
                    }
                }
                flashes += flashed.Count;
            }
            Console.WriteLine($"Done. Flashes: {flashes}");
        }

        public static void Part2()
        {
            int[,] octopuses = new int[10, 10];

            int row = 0;
            int column;
            string input = Console.ReadLine();
            while (input is not null)
            {
                if (input is "done")
                {
                    break;
                }
                column = 0;
                foreach (int number in from character in input
                                       select Int32.Parse($"{character}"))
                {
                    octopuses[column, row] = number;
                    column += 1;
                }
                row += 1;
                input = Console.ReadLine();
            }

            int side = (int)Math.Sqrt(octopuses.Length);
            for (int step = 1; step <= Int32.MaxValue; step += 1)
            {
                List<(int flashedRow, int flashedColumn)> flashed = new();
                for (row = 0; row < side; row += 1)
                {
                    for (column = 0; column < side; column += 1)
                    {
                        if (flashed.Contains((row, column)))
                        {
                            continue;
                        }
                        octopuses[column, row] += 1;
                        if (octopuses[column, row] > 9)
                        {
                            octopuses.PerformFlash(row, column, flashed);
                        }
                    }
                }
                if (octopuses.All(octopus => octopus is 0))
                {
                    Console.WriteLine($"Done. Step: {step}");
                    break;
                }
            }
        }

        public static bool All<T>(this T[,] array, Predicate<T> match)
        {
            foreach (T t in array)
            {
                if (!match(t))
                {
                    return false;
                }
            }
            return true;
        }

        public static void WriteGrid(this int[,] octopuses)
        {
            int side = (int)Math.Sqrt(octopuses.Length);
            for (int row = 0; row < side; row += 1)
            {
                for (int column = 0; column < side; column += 1)
                {
                    Console.Write(octopuses[column, row]);
                }
                Console.Write('\n');
            }
            Console.Write('\n');
        }

        public static void PerformFlash(this int[,] octopuses, int row, int column, List<(int flashedRow, int flashedColumn)> alreadyFlashed)
        {
            octopuses[column, row] = 0;
            alreadyFlashed.Add((row, column));
            foreach ((int adjacentRow, int adjacentColumn) in octopuses.AdjacentOctopuses(row, column))
            {
                if (alreadyFlashed.Contains((adjacentRow, adjacentColumn)))
                {
                    continue;
                }
                octopuses[adjacentColumn, adjacentRow] += 1;
                if (octopuses[adjacentColumn, adjacentRow] > 9)
                {
                    octopuses.PerformFlash(adjacentRow, adjacentColumn, alreadyFlashed);
                }
            }
        }

        public static IEnumerable<(int row, int column)> AdjacentOctopuses(this int[,] octopuses, int row, int column)
        {
            double side = Math.Sqrt(octopuses.Length);
            for (int r = (((row - 1) < 0) ? 0 : row - 1); r <= ((row + 1) >= side ? side - 1 : row + 1); r += 1)
            {
                for (int c = (((column - 1) < 0) ? 0 : column - 1); c <= ((column + 1) >= side ? side - 1 : column + 1); c += 1)
                {
                    if ((r == row) && (c == column))
                    {
                        continue;
                    }
                    yield return (r, c);
                }
            }
        }
    }
}