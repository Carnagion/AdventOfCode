using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

using Aoc.Extensions;

using Bibliographer.Formats.Mla;

namespace Aoc
{
    public static class Day17
    {
        [Author("SuperNinja83")]
        [Container("Reddit")]
        [Location("https://www.reddit.com/r/adventofcode/comments/ri9kdq/comment/hovvvw2/?utm_source=share&utm_medium=web2x&context=3")]
        [Accessed(2021, 12, 17)]
        public static void Part1()
        {
            string input = Console.ReadLine()![13..];
            string[] split = input.Split(", ");
            string[] xCoordinates = split[0][2..].Split("..");
            string[] yCoordinates = split[1][2..].Split("..");
            (int xMin, int xMax, int yMin, int yMax) target = (Int32.Parse(xCoordinates[0]), Int32.Parse(xCoordinates[1]), Int32.Parse(yCoordinates[0]), Int32.Parse(yCoordinates[1]));
            
            Console.WriteLine($"{target.xMin} to {target.xMax}, {target.yMin} to {target.yMax}");

            // maximum y-distance that can be traveled in one step is from target min y to 0
            // that means y velocity previous step (to reach 0 from somewhere) was (target min y - 1)
            // curve formed when launching from 0 until it reaches max then drops back down to 0
            // this curve is a parabola and is symmetrical
            // so initial velocity must also be (target min y - 1)
            // use + 1 instead of - 1 because velocity and target min y are negative in this case
            int maximumYPosition = (target.yMin * (target.yMin + 1)) / 2;
            Console.WriteLine(maximumYPosition);
        }

        public static void Part2()
        {
            string input = Console.ReadLine()![13..];
            string[] split = input.Split(", ");
            string[] xCoordinates = split[0][2..].Split("..");
            string[] yCoordinates = split[1][2..].Split("..");
            (int xMin, int xMax, int yMin, int yMax) target = (Int32.Parse(xCoordinates[0]), Int32.Parse(xCoordinates[1]), Int32.Parse(yCoordinates[0]), Int32.Parse(yCoordinates[1]));
            
            Console.WriteLine($"{target.xMin} to {target.xMax}, {target.yMin} to {target.yMax}");
            Console.WriteLine(Day17.PossibleStartingVelocitiesToReachTarget(new(0, 0), target).LongCount());
            // Console.WriteLine(Day17.PossibleStartingVelocitiesToReachTargetSuvat(new(0, 0), target).LongCount());
        }

        public static Vector2 GetNextVelocity(this Vector2 velocity)
        {
            float x = velocity.X > 0 ? velocity.X - 1 : velocity.X < 0 ? velocity.X + 1 : 0;
            float y = velocity.Y - 1;
            return new(x, y);
        }

        public static bool IsWithinTarget(this Vector2 position, (int xMin, int xMax, int yMin, int yMax) target)
        {
            return position.X.IsBetweenInclusive(target.xMin, target.xMax) && position.Y.IsBetweenInclusive(target.yMin, target.yMax);
        }

        public static bool WillHitTarget(this Vector2 start, Vector2 velocity, (int xMin, int xMax, int yMin, int yMax) target)
        {
            Vector2 current = start;
            while (current.Y >= target.yMin)
            {
                current += velocity;
                velocity = velocity.GetNextVelocity();
                if (current.IsWithinTarget(target))
                {
                    return true;
                }
            }
            return false;
        }

        public static IEnumerable<Vector2> PossibleStartingVelocitiesToReachTarget(Vector2 start, (int xMin, int xMax, int yMin, int yMax) target)
        {
            for (int xCurrent = (int)start.X; xCurrent < (target.xMax + 1); xCurrent += 1)
            {
                for (int yCurrent = target.yMin; yCurrent < -target.yMin; yCurrent += 1)
                {
                    Vector2 velocity = new(xCurrent, yCurrent);
                    if (start.WillHitTarget(velocity, target))
                    {
                        yield return velocity;
                    }
                }
            }
        }

        // unfortunately, suvat does not work
        /*public static (float positive, float negative)? SolveQuadraticEquation(float a, float b, float c)
        {
            float bSquared = b * b;
            float acFour = 4 * a * c;
            if (bSquared < acFour)
            {
                return null;
            }

            float discriminant = (float)Math.Sqrt(bSquared - acFour);
            
            float positive = -b + discriminant;
            float negative = -b - discriminant;

            float aTwo = 2 * a;

            return (positive / aTwo, negative / aTwo);
        }

        public static bool WillHitTargetSuvat(this Vector2 start, Vector2 velocity, (int xMin, int xMax, int yMin, int yMax) target)
        {
            for (int x = target.xMin; x < target.xMax; x += 1)
            {
                float distance = x - start.X;
                (float positive, float _)? root = Day17.SolveQuadraticEquation(-0.5f, -velocity.X, distance);
                if (!root.HasValue)
                {
                    return false;
                }
                if (root.Value.positive.IsInteger())
                {
                    return true;
                }
            }
            
            for (int y = target.yMin; y <= target.yMax; y += 1)
            {
                float distance = y - start.Y;
                (float positive, float _)? root = Day17.SolveQuadraticEquation(-0.5f, -velocity.Y, distance);
                if (!root.HasValue)
                {
                    return false;
                }
                if (root.Value.positive.IsInteger())
                {
                    return true;
                }
            }

            return false;
        }

        public static IEnumerable<Vector2> PossibleStartingVelocitiesToReachTargetSuvat(Vector2 start, (int xMin, int xMax, int yMin, int yMax) target)
        {
            for (int xCurrent = (int)start.X; xCurrent < (target.xMax + 1); xCurrent += 1)
            {
                for (int yCurrent = target.yMin; yCurrent < -target.yMin; yCurrent += 1)
                {
                    Vector2 velocity = new(xCurrent, yCurrent);
                    if (start.WillHitTargetSuvat(velocity, target))
                    {
                        yield return velocity;
                    }
                }
            }
        }*/
    }
}