using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Aoc
{
    public static class Day22
    {
        public static void Part1()
        {
            Dictionary<Cuboid, bool> ranges = new();
            while (true)
            {
                string? input = Console.ReadLine();
                if (input is null or "done")
                {
                    break;
                }
                string[] split = input.Split(' ');
                bool on = split[0] is "on";
                
                string[] coordinates = split[1].Split(',');
                string[] x = coordinates[0][2..].Split("..");
                string[] y = coordinates[1][2..].Split("..");
                string[] z = coordinates[2][2..].Split("..");

                Cuboid cuboid = new(Int32.Parse(x[0]), Int32.Parse(x[1]) + 1, Int32.Parse(y[0]), Int32.Parse(y[1]) + 1, Int32.Parse(z[0]), Int32.Parse(z[1]) + 1);
                if ((cuboid.Minimum.X >= -50) && (cuboid.Maximum.X <= 50) && (cuboid.Minimum.Y >= -50) && (cuboid.Maximum.Y <= 50) && (cuboid.Minimum.Z >= -50) && (cuboid.Maximum.Z <= 50))
                {
                    ranges.Add(cuboid, on);
                }
            }

            List<Cuboid> cuboids = new();
            foreach ((Cuboid current, bool on) in ranges)
            {
                cuboids = cuboids.SelectMany(cuboid => cuboid.Subtract(current)).ToList();
                if (on)
                {
                    cuboids.Add(current);
                }
            }

            long volume = cuboids.Select(cuboid => cuboid.Volume).Aggregate(0L, (current, volume) => current + volume);
            Console.WriteLine($"Done. On: {volume}");
        }

        public static void Part2()
        {
            Dictionary<Cuboid, bool> ranges = new();
            while (true)
            {
                string? input = Console.ReadLine();
                if (input is null or "done")
                {
                    break;
                }
                string[] split = input.Split(' ');
                bool on = split[0] is "on";
                
                string[] coordinates = split[1].Split(',');
                string[] x = coordinates[0][2..].Split("..");
                string[] y = coordinates[1][2..].Split("..");
                string[] z = coordinates[2][2..].Split("..");

                Cuboid cuboid = new(Int32.Parse(x[0]), Int32.Parse(x[1]) + 1, Int32.Parse(y[0]), Int32.Parse(y[1]) + 1, Int32.Parse(z[0]), Int32.Parse(z[1]) + 1);
                ranges.Add(cuboid, on);
            }

            List<Cuboid> cuboids = new();
            foreach ((Cuboid current, bool on) in ranges)
            {
                cuboids = cuboids.SelectMany(cuboid => cuboid.Subtract(current)).ToList();
                if (on)
                {
                    cuboids.Add(current);
                }
            }

            long volume = cuboids.Select(cuboid => cuboid.Volume).Aggregate(0L, (current, volume) => current + volume);
            Console.WriteLine($"Done. On: {volume}");
        }

        public record Cuboid
        {
            public Cuboid(int xMin, int xMax, int yMin, int yMax, int zMin, int zMax)
            {
                this.Minimum = new(xMin, yMin, zMin);
                this.Maximum = new(xMax, yMax, zMax);
            }
            
            public Vector3 Minimum
            {
                get;
            }

            public Vector3 Maximum
            {
                get;
            }

            public long Volume
            {
                get
                {
                    return (long)(this.Maximum.X - this.Minimum.X) * (long)(this.Maximum.Y - this.Minimum.Y) * (long)(this.Maximum.Z - this.Minimum.Z);
                }
            }

            public bool Intersects(Cuboid cuboid)
            {
                return (this.Minimum.X <= cuboid.Maximum.X) && (this.Maximum.X >= cuboid.Minimum.X) && (this.Minimum.Y <= cuboid.Maximum.Y) && (this.Maximum.Y >= cuboid.Minimum.Y) && (this.Minimum.Z <= cuboid.Maximum.Z) && (this.Maximum.Z >= cuboid.Minimum.Z);
            }

            public bool Contains(Cuboid cuboid)
            {
                return (this.Minimum.X <= cuboid.Minimum.X) && (this.Maximum.X >= cuboid.Maximum.X) && (this.Minimum.Y <= cuboid.Minimum.Y) && (this.Maximum.Y >= cuboid.Maximum.Y) && (this.Minimum.Z <= cuboid.Minimum.Z) && (this.Maximum.Z >= cuboid.Maximum.Z);
            }

            public IEnumerable<Cuboid> Subtract(Cuboid cuboid)
            {
                if (cuboid.Contains(this))
                {
                    yield break;
                }

                if (!this.Intersects(cuboid))
                {
                    yield return this;
                    yield break;
                }

                IEnumerable<int> splitX = new[] {(int)cuboid.Minimum.X, (int)cuboid.Maximum.X,}.Where(number => (this.Minimum.X < number) && (number < this.Maximum.X));
                IEnumerable<int> splitY = new[] {(int)cuboid.Minimum.Y, (int)cuboid.Maximum.Y,}.Where(number => (this.Minimum.Y < number) && (number < this.Maximum.Y));
                IEnumerable<int> splitZ = new[] {(int)cuboid.Minimum.Z, (int)cuboid.Maximum.Z,}.Where(number => (this.Minimum.Z < number) && (number < this.Maximum.Z));

                List<int> xValues = new() {(int)this.Minimum.X, (int)this.Maximum.X,};
                xValues.InsertRange(1, splitX);
                List<int> yValues = new() {(int)this.Minimum.Y, (int)this.Maximum.Y,};
                yValues.InsertRange(1, splitY);
                List<int> zValues = new() {(int)this.Minimum.Z, (int)this.Maximum.Z,};
                zValues.InsertRange(1, splitZ);

                for (int x = 0; x < (xValues.Count - 1); x += 1)
                {
                    for (int y = 0; y < (yValues.Count - 1); y += 1)
                    {
                        for (int z = 0; z < (zValues.Count - 1); z += 1)
                        {
                            Cuboid result = new(xValues[x], xValues[x + 1], yValues[y], yValues[y + 1], zValues[z], zValues[z + 1]);
                            if (!cuboid.Contains(result))
                            {
                                yield return result;
                            }
                        }
                    }
                }
            }

            public override string ToString()
            {
                return $"x: {this.Minimum.X}..{this.Maximum.X}, y: {this.Minimum.Y}..{this.Maximum.Y}, z: {this.Minimum.Z}..{this.Maximum.Z}";
            }
        }
    }
}