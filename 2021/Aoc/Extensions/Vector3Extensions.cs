using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Aoc.Extensions
{
    public static class Vector3Extensions
    {
        public static int ManhattanDistanceTo(this Vector3 from, Vector3 to)
        {
            return (int)(Math.Abs(to.X - from.X) + Math.Abs(to.Y - from.Y) + Math.Abs(from.Z - to.Z));
        }

        public static IEnumerable<Vector3> FacingDirections(this Vector3 vector)
        {
            Vector3 current = vector;
            for (int direction = 0; direction < 3; direction += 1)
            {
                yield return current;
                yield return new(-current.X, -current.Y, current.Z);
                current = new(current.Y, current.Z, current.X);
            }
        }

        public static IEnumerable<Vector3> Rotations(this Vector3 vector)
        {
            Vector3 current = vector;
            for (int direction = 0; direction < 4; direction += 1)
            {
                yield return current;
                current = new(current.X, -current.Z, current.Y);
            }
        }

        public static IEnumerable<Vector3> Orientations(this Vector3 vector)
        {
            return vector.FacingDirections().SelectMany(direction => direction.Rotations());
        }

        public static IEnumerable<Vector3> Range(this Vector3 from, Vector3 to)
        {
            for (int x = (int)from.X; x <= to.X; x += 1)
            {
                for (int y = (int)from.Y; y <= to.Y; y += 1)
                {
                    for (int z = (int)from.Z; z <= to.Z; z += 1)
                    {
                        yield return new(x, y, z);
                    }
                }
            }
        }
    }
}