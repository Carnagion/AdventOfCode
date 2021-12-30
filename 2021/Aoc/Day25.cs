using System;
using System.Collections.Generic;

using Aoc.Extensions;

namespace Aoc
{
    public static class Day25
    {
        public static void Part1()
        {
            List<string> list = new();
            while (true)
            {
                string? input = Console.ReadLine();
                if (input is null or "done")
                {
                    break;
                }
                list.Add(input);
            }

            (char[,] cucumbers, int rows, int columns) = list.To2dArray();
            for (int count = 0; ; count += 1)
            {
                bool moved = cucumbers.StepThroughCucumbers(rows, columns);
                Console.WriteLine($"\ncount: {count}");
                if (!moved)
                {
                    break;
                }
            }
        }

        public static void Part2()
        {
            Console.WriteLine("There is no part 2 :)");
        }

        public static bool StepThroughCucumbers(this char[,] cucumbers, in int rows, in int columns)
        {
            return cucumbers.StepThroughCucumbersLeft(rows, columns) | cucumbers.StepThroughCucumbersDown(rows, columns);
        }

        public static bool StepThroughCucumbersLeft(this char[,] cucumbers, in int rows, in int columns)
        {
            bool movement = false;
            for (int row = 0; row < rows; row += 1)
            {
                char[] moved = new char[columns];
                for (int column = 0; column < columns; column += 1)
                {
                    moved[column] = cucumbers[column, row];
                    
                    char currentCucumber = cucumbers[column, row];
                    if (currentCucumber is not '>')
                    {
                        continue;
                    }
                    
                    int next = column == (columns - 1) ? 0 : column + 1;
                    char nextCucumber = cucumbers[next, row];
                    if (nextCucumber is not '.')
                    {
                        continue;
                    }
                    
                    moved[next] = '>';
                    moved[column] = '.';
                    column += 1;
                    movement = true;
                }
                for (int column = 0; column < columns; column += 1)
                {
                    cucumbers[column, row] = moved[column];
                }
            }
            return movement;
        }

        public static bool StepThroughCucumbersDown(this char[,] cucumbers, in int rows, in int columns)
        {
            bool movement = false;
            for (int column = 0; column < columns; column += 1)
            {
                char[] moved = new char[rows];
                for (int row = 0; row < rows; row += 1)
                {
                    moved[row] = cucumbers[column, row];
                    
                    char currentCucumber = cucumbers[column, row];
                    if (currentCucumber is not 'v')
                    {
                        continue;
                    }
                    
                    int next = row == (rows - 1) ? 0 : row + 1;
                    char nextCucumber = cucumbers[column, next];
                    if (nextCucumber is not '.')
                    {
                        continue;
                    }

                    moved[next] = 'v';
                    moved[row] = '.';
                    row += 1;
                    movement = true;
                }
                for (int row = 0; row < rows; row += 1)
                {
                    cucumbers[column, row] = moved[row];
                }
            }
            return movement;
        }
    }
}