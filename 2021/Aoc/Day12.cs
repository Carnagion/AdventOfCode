using System;
using System.Collections.Generic;
using System.Linq;

using Bibliographer.Formats.Mla;

namespace Aoc
{
    [Author("schovanec")]
    [Container("GitHub")]
    [Location("https://github.com/schovanec/AdventOfCode/blob/master/2021/Day12/Program.cs")]
    [Accessed(2021, 12, 12)]
    public static class Day12
    {
        public static void Part1()
        {
            List<string> lines = new();
            string input = Console.ReadLine();
            while (input is not null)
            {
                if (input is "done")
                {
                    break;
                }
                lines.Add(input);
                input = Console.ReadLine();
            }

            ILookup<string, string> connections = (from line in lines
                                                   select line.Split('-')).SelectMany(split => new[] {(from: split[0], to: split[1]), (from: split[1], to: split[0]),}).ToLookup(connection => connection.from, connection => connection.to);
            Console.WriteLine($"Done. Path count: {connections.FindPaths("start", "end", new(), Day12.CanVisitNodePart1).Count()}");
        }

        public static void Part2()
        {
            List<string> lines = new();
            string input = Console.ReadLine();
            while (input is not null)
            {
                if (input is "done")
                {
                    break;
                }
                lines.Add(input);
                input = Console.ReadLine();
            }

            ILookup<string, string> connections = (from line in lines
                                                   select line.Split('-')).SelectMany(split => new[] {(from: split[0], to: split[1]), (from: split[1], to: split[0]),}).ToLookup(connection => connection.from, connection => connection.to);
            Console.WriteLine($"Done. Path count: {connections.FindPaths("start", "end", new(), Day12.CanVisitNodePart2).Count()}");
        }

        public static IEnumerable<Path> FindPaths(this ILookup<string, string> connections, string start, string end, Dictionary<string, int> visited, Func<string, Dictionary<string, int>, bool> canVisitNode)
        {
            visited[start] = visited.GetValueOrDefault(start) + 1;

            Path path = new(start);
            if (start == end)
            {
                yield return path;
            }
            else
            {
                foreach (string node in from connection in connections[start]
                                        where canVisitNode(connection, visited)
                                        select connection)
                {
                    foreach (Path remaining in connections.FindPaths(node, end, visited, canVisitNode))
                    {
                        yield return path with
                        {
                            RemainingPath = remaining,
                        };
                    }
                }
            }
            visited[start] -= 1;
        }

        public static bool CanVisitNodePart1(string node, Dictionary<string, int> visited)
        {
            return node.IsLargeCave() || visited.GetValueOrDefault(node) is 0;
        }

        public static bool CanVisitNodePart2(string node, Dictionary<string, int> visited)
        {
            switch (node)
            {
                case "start" or "end":
                    return visited.GetValueOrDefault(node) is 0;
                default:
                    if (node.IsSmallCave())
                    {
                        return visited.GetValueOrDefault(node) is 0 || !visited.Any(entry => entry.Key.IsSmallCave() && (entry.Value > 1));
                    }
                    return true;
            }
        }

        public static bool IsLargeCave(this string node)
        {
            return node.All(character => Char.IsUpper(character));
        }

        public static bool IsSmallCave(this string node)
        {
            return node is not ("start" or "end") && !node.IsLargeCave();
        }

        public record Path
        {
            public Path(string start, Path remaining = null)
            {
                this.Start = start;
                this.RemainingPath = remaining;
            }

            public string Start
            {
                get;
            }

            public Path RemainingPath
            {
                get;
                set;
            }
        }
    }
}