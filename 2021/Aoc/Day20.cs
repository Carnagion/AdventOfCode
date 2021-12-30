using System;
using System.Collections.Generic;
using System.Linq;

using Aoc.Extensions;

namespace Aoc
{
    public static class Day20
    {
        public static void Part1()
        {
            string enhancer = Console.ReadLine()!;

            List<string> imageList = new();
            while (true)
            {
                string? input = Console.ReadLine();
                if (input is null or "done")
                {
                    break;
                }
                imageList.Add(input);
            }

            bool dark = true;
            for (int count = 0; count < 2; count += 1)
            {
                imageList.Pad(dark ? '.' : '#', 2);
                Console.WriteLine("\nImage: ");
                foreach (string line in imageList)
                {
                    Console.WriteLine(line);
                }
                
                (char[,] image, int rows, int columns) = imageList.To2dArray();
                
                char[,] enhanced = Day20.EnhanceImage(image, rows, columns, enhancer);

                imageList = enhanced.ToList(rows - 2, columns - 2);
                Console.WriteLine("\nReconverted: ");
                foreach (string line in imageList)
                {
                    Console.WriteLine(line);
                }

                long lit = enhanced.Cast<char>().LongCount(pixel => pixel is '#');
                Console.WriteLine($"Lit pixels: {lit}");
                dark = !dark;
            }
        }

        public static void Part2()
        {
            string enhancer = Console.ReadLine()!;

            List<string> imageList = new();
            while (true)
            {
                string? input = Console.ReadLine();
                if (input is null or "done")
                {
                    break;
                }
                imageList.Add(input);
            }

            bool dark = true;
            for (int count = 0; count < 50; count += 1)
            {
                imageList.Pad(dark ? '.' : '#', 2);
                Console.WriteLine("\nImage: ");
                foreach (string line in imageList)
                {
                    Console.WriteLine(line);
                }
                
                (char[,] image, long rows, long columns) = imageList.To2dArray();
                
                char[,] enhanced = Day20.EnhanceImage(image, rows, columns, enhancer);

                imageList = enhanced.ToList(rows - 2, columns - 2);
                Console.WriteLine("\nReconverted: ");
                foreach (string line in imageList)
                {
                    Console.WriteLine(line);
                }

                long lit = enhanced.Cast<char>().LongCount(pixel => pixel is '#');
                Console.WriteLine($"Lit pixels: {lit}");
                dark = !dark;
            }
        }

        public static char[,] EnhanceImage(char[,] image, long rows, long columns, string enhancer)
        {
            char[,] enhanced = new char[rows - 2, columns - 2];
            for (long row = 1; row < (rows - 1); row += 1)
            {
                for (long column = 1; column < (columns - 1); column += 1)
                {
                    string adjacent = new(image.Adjacent(row, column).ToArray());
                    string binary = Day20.ConvertHashesAndDotsToBinary(adjacent);
                    int index = Convert.ToInt32(binary, 2);
                    
                    enhanced[column - 1, row - 1] = enhancer[index];
                }
            }
            return enhanced;
        }

        public static string ConvertHashesAndDotsToBinary(string expression)
        {
            return expression.Replace('.', '0').Replace('#', '1');
        }
    }
}