using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

using Aoc.Extensions;

using Bibliographer.Formats.Mla;

namespace Aoc
{
    public static class Day15
    {
        public static void Part1()
        {
            int[,] graph = null;

            string input = null;
            int size = 0;
            int row = 0;
            int column = 0;
            do
            {
                input = Console.ReadLine();
                if (input is "done")
                {
                    break;
                }
                
                if (graph is null)
                {
                    size = input!.Length;
                    graph = new int[size, size];
                }

                for (column = 0; column < size; column += 1)
                {
                    graph[column, row] = Int32.Parse($"{input![column]}");
                }
                row += 1;
            } while (input is not null);

            Vector2 start = Vector2.Zero;
            Vector2 end = new(size - 1, size - 1);
            int distance = graph.PathFindDijkstra(size, start, end);
            Console.WriteLine($"Done. Start: {start}. End: {end}. Distance: {distance}");
        }
        
        public static void Part2()
        {
            int[,] graph = null;

            string input = null;
            int size = 0;
            int row = 0;
            int column = 0;
            do
            {
                input = Console.ReadLine();
                if (input is "done")
                {
                    break;
                }
                
                if (graph is null)
                {
                    size = input!.Length;
                    graph = new int[size, size];
                }

                for (column = 0; column < size; column += 1)
                {
                    graph[column, row] = Int32.Parse($"{input![column]}");
                }
                row += 1;
            } while (input is not null);

            Vector2 start = Vector2.Zero;
            Vector2 end = new((size * 5) - 1, (size * 5) - 1);

            int[,] expansion = graph.Expand(5, number => number is 9 ? 1 : number + 1);
            int distance = expansion.PathFindDijkstra(size * 5, start, end);
            Console.WriteLine($"Done. Start: {start}. End: {end}. Distance: {distance}");
        }

        [Author("mebeim")]
        [Title("mebeim/aoc")]
        [Container("GitHub")]
        [Location("https://github.com/mebeim/aoc/blob/master/2021/README.md#day-15---chiton")]
        [Accessed(2021, 12, 15)]
        public static int PathFindDijkstra(this int[,] graph, int size, Vector2 start, Vector2 end)
        {
            List<Vector2> visited = new();
            Dictionary<Vector2, int> queue = new() {{start, 0},};
            Dictionary<Vector2, int> distances = new();

            for (int row = 0; row < size; row += 1)
            {
                for (int column = 0; column < size; column += 1)
                {
                    distances[new(column, row)] = Int32.MaxValue;
                }
            }
            distances[start] = 0;

            while (queue.Count is not 0)
            {
                (Vector2 lowest, int distance) = queue.RemoveKeyWithLowestValue();
                if (lowest == end)
                {
                    return distance;
                }

                if (visited.Contains(lowest))
                {
                    continue;
                }
                visited.Add(lowest);

                foreach (Vector2 neighbour in graph.NeighbouringCells(size, lowest))
                {
                    int neighbourDistance = distance + graph[(int)neighbour.Y, (int)neighbour.X];
                    if (neighbourDistance < distances[neighbour])
                    {
                        distances[neighbour] = neighbourDistance;
                        queue[neighbour] = neighbourDistance;
                    }
                }
            }
            return Int32.MaxValue;
        }

        public static IEnumerable<Vector2> NeighbouringCells(this int[,] graph, int size, Vector2 cell)
        {
            Vector2[] directions = {new(1, 0), new(-1, 0), new(0, 1), new(0, -1),};
            foreach (Vector2 direction in directions)
            {
                Vector2 neighbour = new(cell.X + direction.X, cell.Y + direction.Y);
                if ((0 <= neighbour.Y) && (neighbour.Y < size) && (0 <= neighbour.X) && (neighbour.X < size))
                {
                    yield return neighbour;
                }
            }
        }

        public static (T, int) RemoveKeyWithLowestValue<T>(this Dictionary<T, int> dictionary)
        {
            (T, int) lowest = (dictionary.First().Key, dictionary.First().Value);
            foreach ((T key, int value) in dictionary)
            {
                if (dictionary[key] < dictionary[lowest.Item1])
                {
                    lowest = (key, value);
                }
            }
            dictionary.Remove(lowest.Item1);
            return lowest;
        }
    }
}