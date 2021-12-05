using System;
using System.Collections.Generic;
using System.Linq;

namespace Aoc
{
    public static class Day5
    {
        public static void Part1()
        {
            List<Segment> segments = new();

            string input = Console.ReadLine();
            while (input is not null)
            {
                if (input is "done")
                {
                    break;
                }

                string[] split = input.Split(" ");
                int[] toPoint = (from string number in split[0].Split(",")
                                 select Int32.Parse(number)).ToArray();
                int[] fromPoint = (from string number in split[2].Split(",")
                                   select Int32.Parse(number)).ToArray();
                Point to = new(toPoint[0], toPoint[1]);
                Point from = new(fromPoint[0], fromPoint[1]);
                segments.Add(new(to, from));

                input = Console.ReadLine();
            }

            List<Point> intersections = new();
            for (int index = 0; index < segments.Count; index += 1)
            {
                if (!(segments[index].IsHorizontal || segments[index].IsVertical))
                {
                    continue;
                }
                for (int remaining = index + 1; remaining < segments.Count; remaining += 1)
                {
                    if (!(segments[remaining].IsHorizontal || segments[remaining].IsVertical))
                    {
                        continue;
                    }
                    if (segments[index].Intersects(segments[remaining]))
                    {
                        intersections.AddRange(from Point point in segments[index].IntersectingPointsWith(segments[remaining])
                                               where !intersections.Contains(point)
                                               select point);
                    }
                }
            }
            
            Console.WriteLine($"Done. Intersections: {intersections.Count}");
        }

        public static void Part2()
        {
            List<Segment> segments = new();

            string input = Console.ReadLine();
            while (input is not null)
            {
                if (input is "done")
                {
                    break;
                }

                string[] split = input.Split(" ");
                int[] toPoint = (from string number in split[0].Split(",")
                                 select Int32.Parse(number)).ToArray();
                int[] fromPoint = (from string number in split[2].Split(",")
                                   select Int32.Parse(number)).ToArray();
                Point to = new(toPoint[0], toPoint[1]);
                Point from = new(fromPoint[0], fromPoint[1]);
                segments.Add(new(to, from));

                input = Console.ReadLine();
            }

            List<Point> intersections = new();
            for (int index = 0; index < segments.Count; index += 1)
            {
                for (int remaining = index + 1; remaining < segments.Count; remaining += 1)
                {
                    if (segments[index].Intersects(segments[remaining]))
                    {
                        intersections.AddRange(from Point point in segments[index].IntersectingPointsWith(segments[remaining])
                                               where !intersections.Contains(point)
                                               select point);
                    }
                }
            }
            
            Console.WriteLine($"Done. Intersections: {intersections.Count}");
        }

        public readonly struct Point
        {
            public Point(int x, int y)
            {
                this.X = x;
                this.Y = y;
            }
            
            public int X
            {
                get;
            }

            public int Y
            {
                get;
            }

            public static int Orientation(Point point1, Point point2, Point point3)
            {
                int orientation = ((point2.Y - point1.Y) * (point3.X - point2.X)) - ((point2.X - point1.X) * (point3.Y - point2.Y));
                return (orientation is 0) ? 0 : (orientation > 0) ? 1 : -1;
            }

            public bool IsOnSegment(Segment segment)
            {
                return ((this.X >= Math.Min(segment.From.X, segment.To.X)) && (this.X <= Math.Max(segment.From.X, segment.To.X)) && (this.Y >= Math.Min(segment.From.Y, segment.To.Y)) && (this.Y <= Math.Max(segment.From.Y, segment.To.Y)));
            }

            public override string ToString()
            {
                return $"({this.X},{this.Y})";
            }
        }

        public readonly struct Segment
        {
            public Segment(Point from, Point to)
            {
                this.From = from;
                this.To = to;
            }

            public Point From
            {
                get;
            }

            public Point To
            {
                get;
            }

            public IEnumerable<Point> Points
            {
                get
                {
                    int xChange = (this.From.X == this.To.X) ? 0 : (this.From.X < this.To.X) ? 1 : -1;
                    int yChange = (this.From.Y == this.To.Y) ? 0 : (this.From.Y < this.To.Y) ? 1 : -1;
                    
                    Point point = this.From;
                    while (true)
                    {
                        if ((point.X == this.To.X) && (point.Y == this.To.Y))
                        {
                            yield return point;
                            break;
                        }
                        yield return point;
                        point = new(point.X + xChange, point.Y + yChange);
                    }
                }
            }

            public bool IsHorizontal
            {
                get
                {
                    return this.From.Y == this.To.Y;
                }
            }

            public bool IsVertical
            {
                get
                {
                    return this.From.X == this.To.X;
                }
            }

            public bool Intersects(Segment segment)
            {
                int orientation1 = Point.Orientation(this.From, this.To, segment.From);
                int orientation2 = Point.Orientation(this.From, this.To, segment.To);
                int orientation3 = Point.Orientation(segment.From, segment.To, this.From);
                int orientation4 = Point.Orientation(segment.From, segment.To, this.To);

                if ((orientation1 != orientation2) && (orientation3 != orientation4))
                {
                    return true;
                }

                if (orientation1 is 0 && segment.From.IsOnSegment(this))
                {
                    return true;
                }
                if (orientation2 is 0 && segment.To.IsOnSegment(this))
                {
                    return true;
                }
                if (orientation3 is 0 && this.From.IsOnSegment(segment))
                {
                    return true;
                }
                if (orientation4 is 0 && this.To.IsOnSegment(segment))
                {
                    return true;
                }
                return false;
            }

            public IEnumerable<Point> IntersectingPointsWith(Segment segment)
            {
                return from Point point in this.Points
                       where segment.Points.Contains(point)
                       select point;
            }

            public override string ToString()
            {
                return $"{this.From} to {this.To}";
            }
        }
    }
}