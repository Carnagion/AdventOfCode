using System;

namespace Aoc
{
    public static class Day2
    {
        public static void Part1()
        {
            int position = 0;
            int depth = 0;
            
            string input = Console.ReadLine();
            while (input is not null)
            {
                if (input is "done")
                {
                    Console.WriteLine($"Done. Position: {position}. Depth: {depth}");
                    break;
                }
                
                string[] parts = input.Split(" ");
                int value = Int32.Parse(parts[1]);
                switch (parts[0])
                {
                    case "forward":
                        position += value;
                        break;
                    case "down":
                        depth += value;
                        break;
                    case "up":
                        depth -= value;
                        break;
                }
                
                input = Console.ReadLine();
            }
        }

        public static void Part2()
        {
            int aim = 0;
            int position = 0;
            int depth = 0;

            string input = Console.ReadLine();
            while (input is not null)
            {
                if (input is "done")
                {
                    Console.WriteLine($"Done. Aim: {aim}. Position: {position}. Depth: {depth}");
                    break;
                }

                string[] parts = input.Split(" ");
                int value = Int32.Parse(parts[1]);
                switch (parts[0])
                {
                    case "forward":
                        position += value;
                        depth += (aim * value);
                        break;
                    case "down":
                        aim += value;
                        break;
                    case "up":
                        aim -= value;
                        break;
                }

                input = Console.ReadLine();
            }
        }
    }
}