using System;
using System.Collections.Generic;
using System.Linq;

namespace Aoc
{
    public static class Day9
    {
        public static void Part1()
        {
            Grid<int> grid = new();

            int row = 0;
            int column = 0;
            string input = Console.ReadLine();
            while (input is not null)
            {
                if (input is "done")
                {
                    break;
                }

                for (column = 0; column < input.Length; column += 1)
                {
                    grid.Add(new(column, row, Int32.Parse(input[column].ToString())));
                }

                input = Console.ReadLine();
                row += 1;
            }

            IEnumerable<int> lowest = from point in grid
                                      let adjacents = grid.AdjacentPointsTo(point)
                                      where adjacents.All(adjacent => point.Data < adjacent.Data)
                                      select point.Data + 1;
            Console.WriteLine($"Done. Risk level: {lowest.Sum()}");
        }

        public static void Part2()
        {
            Grid<int> grid = new();

            int row = 0;
            int column = 0;
            string input = Console.ReadLine();
            while (input is not null)
            {
                if (input is "done")
                {
                    break;
                }

                for (column = 0; column < input.Length; column += 1)
                {
                    grid.Add(new(column, row, Int32.Parse(input[column].ToString())));
                }

                input = Console.ReadLine();
                row += 1;
            }

            IEnumerable<Point<int>> lowest = from point in grid
                                             let adjacents = grid.AdjacentPointsTo(point)
                                             where adjacents.All(adjacent => point.Data < adjacent.Data)
                                             select point;
            IEnumerable<IEnumerable<Point<int>>> basins = (from lowPoint in lowest
                                                           select grid.BasinAt(lowPoint)).OrderByDescending(basin => basin.Count());
            foreach (IEnumerable<Point<int>> basin in basins)
            {
                Console.WriteLine($"Count: {basin.Count()}");
            }
        }

        public static IEnumerable<Point<int>> BasinAt(this Grid<int> grid, Point<int> point)
        {
            if (point.Data is 9)
            {
                return Enumerable.Empty<Point<int>>();
            }

            List<Point<int>> visited = new() {point,};
            return grid.UnvisitedConnectedPointsTo(point, visited);
        }

        public static IEnumerable<Point<int>> UnvisitedConnectedPointsTo(this Grid<int> grid, Point<int> point, List<Point<int>> visited)
        {
            foreach (Point<int> adjacent in from adjacent in grid.AdjacentPointsTo(point)
                                            where adjacent.Data is not 9 && !visited.Contains(adjacent)
                                            select adjacent)
            {
                visited.Add(adjacent);
                grid.UnvisitedConnectedPointsTo(adjacent, visited);
            }
            return visited;
        }

        public readonly struct Point<T>
        {
            public Point(int x, int y, T data)
            {
                this.X = x;
                this.Y = y;
                this.Data = data;
            }
            
            public int X
            {
                get;
            }

            public int Y
            {
                get;
            }

            public T Data
            {
                get;
            }
        }

        public class Grid<T> : List<Point<T>>
        {
            public IEnumerable<Point<T>> AdjacentPointsTo(Point<T> point)
            {
                return from coordinate in this
                       where (Math.Abs(coordinate.X - point.X) is 1 && Math.Abs(coordinate.Y - point.Y) is 0) ^ (Math.Abs(coordinate.Y - point.Y) is 1 && Math.Abs(coordinate.X - point.X) is 0)
                       select coordinate;
            }
        }
    }
}