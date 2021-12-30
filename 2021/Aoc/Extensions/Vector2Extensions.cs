using System;
using System.Numerics;

namespace Aoc.Extensions
{
    public static class Vector2Extensions
    {
        public static int ManhattanDistance(this Vector2 from, Vector2 to)
        {
            return (int)Math.Abs(from.X - to.X) + (int)Math.Abs(from.Y - to.Y);
        }
    }
}