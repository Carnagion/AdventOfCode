using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

using Aoc.Extensions;

using Bibliographer.Formats.Mla;

namespace Aoc
{
    public static class Day19
    {
        public static void Part1()
        {
            List<string> lines = new();
            while (true)
            {
                string? input = Console.ReadLine();
                if (input is null or "done")
                {
                    break;
                }
                lines.Add(input);
            }

            IEnumerable<Scanner> scanners = Scanner.ParseAsScanners(lines);
            IEnumerable<Scanner> matched = Scanner.AlignAll(scanners);
            
            IEnumerable<Vector3> unique = matched.Select(scanner => scanner.AbsoluteBeaconPositions)
                .Aggregate((beacons1, beacons2) => beacons1.Union(beacons2));
            
            Console.WriteLine($"Done. Beacon count: {unique.Count()}");
        }

        public static void Part2()
        {
            List<string> lines = new();
            while (true)
            {
                string? input = Console.ReadLine();
                if (input is null or "done")
                {
                    break;
                }
                lines.Add(input);
            }

            IEnumerable<Scanner> scanners = Scanner.ParseAsScanners(lines);
            IEnumerable<Scanner> matched = Scanner.AlignAll(scanners);

            IEnumerable<int> distances = from scanner1 in matched
                                         from scanner2 in matched
                                         where scanner1 != scanner2
                                         select scanner1.Position.ManhattanDistanceTo(scanner2.Position);
            Console.WriteLine($"Done. Max distance: {distances.Max()}");

        }

        [Author("schovanec")]
        [Title("schovanec/AdventOfCode")]
        [Container("GitHub")]
        [Location("https://github.com/schovanec/AdventOfCode/blob/master/2021/Day19/Program.cs")]
        [Accessed(2021, 12, 19)]
        public record Scanner
        {
            public Scanner(int id, HashSet<Vector3> relativeBeaconPositions, Vector3 position = default)
            {
                this.Id = id;
                this.RelativeBeaconPositions = relativeBeaconPositions;
                this.Position = position;
            }
            
            public int Id
            {
                get;
            }

            public Vector3 Position
            {
                get;
                private set;
            }

            public HashSet<Vector3> RelativeBeaconPositions
            {
                get;
            }

            public IEnumerable<Vector3> AbsoluteBeaconPositions
            {
                get
                {
                    return from position in this.RelativeBeaconPositions
                           select position + this.Position;
                }
            }

            public static IEnumerable<Scanner> AlignAll(IEnumerable<Scanner> scanners)
            {
                Dictionary<int, Scanner> matched = new();
                Dictionary<int, IEnumerable<Scanner>> unmatched = scanners.GroupBy(scanner => scanner.Id)
                    .ToDictionary(group => group.Key, group => group.AsEnumerable());

                matched.Add(0, unmatched[0].First());
                unmatched.Remove(0);

                Queue<int> queue = new();
                queue.Enqueue(0);

                while (queue.Any() && unmatched.Any())
                {
                    int id = queue.Dequeue();
                    IEnumerable<Scanner> matches = Scanner.AlignAnyScanner(matched[id], unmatched.Values);
                    foreach (Scanner match in matches)
                    {
                        matched[match.Id] = match;
                        queue.Enqueue(match.Id);
                        unmatched.Remove(match.Id);
                    }
                }

                return matched.Values;
            }

            public static IEnumerable<Scanner> AlignAnyScanner(Scanner target, IEnumerable<IEnumerable<Scanner>> scanners)
            {
                return scanners.SelectMany(group => Scanner.AlignAnyOrientation(target, group));
            }

            public static IEnumerable<Scanner> AlignAnyOrientation(Scanner target, IEnumerable<Scanner> scanners)
            {
                return scanners.SelectMany(scanner => Scanner.AlignSingleScanner(target, scanner)).Take(1);
            }

            public static IEnumerable<Scanner> AlignSingleScanner(Scanner target, Scanner scanner)
            {
                return (from absolute in target.AbsoluteBeaconPositions
                        from relative in scanner.RelativeBeaconPositions
                        let offset = absolute - relative
                        let updated = scanner with
                        {
                            Position = offset,
                        }
                        where target.AbsoluteBeaconPositions.Intersect(updated.AbsoluteBeaconPositions).Count() >= 12
                        select updated).Take(1);
            }

            public static IEnumerable<Scanner> GetOrientations(int id, IEnumerable<Vector3> beacons)
            {
                return beacons.SelectMany(beacon => beacon.Orientations().Select((vector, index) => (index, vector)))
                    .GroupBy(pair => pair.index, pair => pair.vector)
                    .Select(group => new Scanner(id, group.ToHashSet()));
            }

            public static IEnumerable<Scanner> ParseAsScanners(IEnumerable<string> input)
            {
                List<Scanner> scanners = new();
                List<Vector3> beacons = new();
                int id = -1;
                foreach (string line in input)
                {
                    if (line.StartsWith("---"))
                    {
                        if (beacons.Any())
                        {
                            scanners.AddRange(Scanner.GetOrientations(id, beacons));
                        }
                        beacons.Clear();
                        id += 1;
                    }
                    else if ((id >= 0) && !String.IsNullOrEmpty(line))
                    {
                        string[] coordinates = line.Split(',');
                        beacons.Add(new(Int32.Parse(coordinates[0]), Int32.Parse(coordinates[1]), Int32.Parse(coordinates[2])));
                    }
                }
                if (beacons.Any())
                {
                    scanners.AddRange(Scanner.GetOrientations(id, beacons));
                }
                return scanners;
            }
        }
    }
}