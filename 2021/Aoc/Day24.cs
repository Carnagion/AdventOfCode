using System;
using System.Collections.Generic;
using System.Linq;

using Bibliographer.Formats.Mla;

namespace Aoc
{
    public static class Day24
    {
        public static void Part1()
        {
            Day24.SolveAlu();
            
            /*List<string>[] instructions = new List<string>[14];
            int index = -1;
            while (true)
            {
                string? input = Console.ReadLine();
                if (input is null or "done")
                {
                    break;
                }
                
                if (input[..3] is "inp")
                {
                    index += 1;
                    instructions[index] = new();
                }
                instructions[index].Add(input);
            }

            Alu[] alus = new Alu[14];
            for (index = 0; index < 14; index += 1)
            {
                alus[index] = new();
            }

            for (long model = 99999999999999; model >= 11111111111111; model -= 1)
            {
                string number = model.ToString();
                if (number.Contains('0'))
                {
                    continue;
                }
                
                Console.WriteLine(number);
                for (index = 0; index < number.Length; index += 1)
                {
                    alus[index].Input = Convert.ToInt64(number[index]);
                    alus[index].Reset();
                    if (!alus[index].RunInstructions(instructions[index]))
                    {
                        break;
                    }
                }
                if (alus.All(alu => alu.Variables['z'] is 0))
                {
                    Console.WriteLine($"Done. Number: {model}");
                    break;
                }
            }*/
        }

        public static void Part2()
        {
            Day24.SolveAlu();
        }

        [Author("Gabba333")]
        [Container("GitHub")]
        [Location("https://www.reddit.com/r/adventofcode/comments/rnejv5/comment/hpv6xz7/?utm_source=share&utm_medium=web2x&context=3")]
        [Accessed(2021, 12, 25)]
        public static void SolveAlu()
        {
            List<string>? steps = new();
            while (true)
            {
                string? input = Console.ReadLine();
                if (input is null or "done")
                {
                    break;
                }
                steps.Add(input);
            }
            
            (int, int, int)[] ps = new (int, int, int)[14];
            for (int i = 0; i < 14; i++)
            {
                ps[i] = (Int32.Parse(new string(steps[4 + (i * 18)]).Skip(6).ToArray()), Int32.Parse(new(steps[5 + (i * 18)].Skip(6).ToArray())), Int32.Parse(new(steps[15 + (i * 18)].Skip(6).ToArray())));
            }

            (long? lowest, long? highest) = (Int64.MaxValue, Int64.MinValue);
            for (long i = 10000000000000; i <= 99999999999999; i++)
            {
                int[] digits = i.ToString().Select(ch => Int32.Parse(ch.ToString())).ToArray();
                int step = 0;
                long z = 0;

                foreach ((int p1, int p2, int p3) in ps)
                {
                    int w = digits[step];
                    bool test = ((z % 26) + p2) == w;
                    if ((w != 0) && (p1 == 26) && test)
                    {
                        z /= p1;
                    }
                    else if ((w != 0) && (p1 == 1) && !test)
                    {
                        z = (26 * (z / p1)) + w + p3;
                    }
                    else
                    {
                        i += (long) Math.Pow(10, 13 - step);
                        i--;
                        break;
                    }
                    step++;
                }

                if (z == 0)
                {
                    (lowest, highest) = (i < lowest ? i : lowest, i > highest ? i : highest);
                }
            }
            Console.WriteLine((lowest, highest));
        }

        public class Alu
        {
            public Dictionary<char, long> Variables
            {
                get;
            } = new()
            {
                {'w', 0},
                {'x', 0},
                {'y', 0},
                {'z', 0},
            };

            public long Input
            {
                get;
                set;
            }

            public static bool ValidateOperand(string operand)
            {
                return operand switch
                {
                    "w" => true,
                    "x" => true,
                    "y" => true,
                    "z" => true,
                    _ => false,
                };
            }

            public void Reset()
            {
                foreach (KeyValuePair<char, long> entry in this.Variables)
                {
                    this.Variables[entry.Key] = 0;
                }
            }

            public bool RunInstructions(IEnumerable<string> instructions)
            {
                foreach (string instruction in instructions)
                {
                    Console.WriteLine(instruction);
                    this.ParseInstruction(instruction);
                }
                return this.Variables['z'] is 0;
            }

            public void ParseInstruction(string instruction)
            {
                string[] split = instruction.Split(' ');
                
                string operation = split[0];
                char operand1 = split[1][0];
                
                if (split.Length is 2)
                {
                    this.Variables[operand1] = this.Input;
                    this.Input = 0;
                    return;
                }

                string operand2 = split[2];

                switch (operation)
                {
                    case "add":
                        if (Alu.ValidateOperand(operand2))
                        {
                            this.Variables[operand1] += this.Variables[operand2[0]];
                        }
                        else
                        {
                            this.Variables[operand1] += Int64.Parse(operand2);
                        }
                        break;
                    case "mul":
                        if (Alu.ValidateOperand(operand2))
                        {
                            this.Variables[operand1] *= this.Variables[operand2[0]];
                        }
                        else
                        {
                            this.Variables[operand1] *= Int64.Parse(operand2);
                        }
                        break;
                    case "div":
                        if (Alu.ValidateOperand(operand2))
                        {
                            this.Variables[operand1] /= this.Variables[operand2[0]];
                        }
                        else
                        {
                            this.Variables[operand1] /= Int64.Parse(operand2);
                        }
                        break;
                    case "mod":
                        if (Alu.ValidateOperand(operand2))
                        {
                            this.Variables[operand1] %= this.Variables[operand2[0]];
                        }
                        else
                        {
                            this.Variables[operand1] %= Int64.Parse(operand2);
                        }
                        break;
                    case "eql":
                        if (Alu.ValidateOperand(operand2))
                        {
                            this.Variables[operand1] = this.Variables[operand1] == this.Variables[operand2[0]] ? 1 : 0;
                        }
                        else
                        {
                            this.Variables[operand1] = this.Variables[operand1] == Int64.Parse(operand2) ? 1 : 0;
                        }
                        break;
                }
            }

            public override string ToString()
            {
                return $"w: {this.Variables['w']}, x: {this.Variables['x']}, y: {this.Variables['y']}, z: {this.Variables['z']}";
            }
        }
    }
}