using System;
using System.Collections.Generic;
using System.Linq;

using Bibliographer.Formats.Mla;

namespace Aoc
{
    public static class Day21
    {
        public static void Part1()
        {
            int player1 = Int32.Parse(Console.ReadLine()![^1].ToString());
            int player2 = Int32.Parse(Console.ReadLine()![^1].ToString());

            DiracDiceBoard board = new(player1, player2);
            int rolls = 0;
            while (true)
            {
                int sum;
                
                sum = 0;
                for (int count = 0; count < 3; count += 1)
                {
                    board.RollDice();
                    sum += board.Dice;
                    Console.WriteLine($"Dice: {board.Dice}");
                    rolls += 1;
                }
                board.Players[0].UpdateScore(sum);
                Console.WriteLine($"Player 1: {board.Players[0].Score} (place {board.Players[0].Place})");

                if (board.Players[0].Score >= 1000)
                {
                    break;
                }
                
                sum = 0;
                for (int count = 0; count < 3; count += 1)
                {
                    board.RollDice();
                    sum += board.Dice;
                    Console.WriteLine($"Dice: {board.Dice}");
                    rolls += 1;
                }
                board.Players[1].UpdateScore(sum);
                Console.WriteLine($"Player 2: {board.Players[1].Score} (place {board.Players[1].Place})");

                if (board.Players[1].Score >= 1000)
                {
                    break;
                }
            }
            Console.WriteLine($"Rolls: {rolls}");
        }

        [Author("constb")]
        [Title("constb/aoc2021")]
        [Container("GitHub")]
        [Location("https://github.com/constb/aoc2021/blob/9b96870591f252cffbff2d28f0b49917f224550e/21/index2.js#L43")]
        [Accessed(2021, 12, 21)]
        public static void Part2()
        {
            int[,] outcomes = {
                {1, 1, 1,},
                {1, 1, 2,},
                {1, 1, 3,},
                {1, 2, 1,},
                {1, 2, 2,},
                {1, 2, 3,},
                {1, 3, 1,},
                {1, 3, 2,},
                {1, 3, 3,},
                {2, 1, 1,},
                {2, 1, 2,},
                {2, 1, 3,},
                {2, 2, 1,},
                {2, 2, 2,},
                {2, 2, 3,},
                {2, 3, 1,},
                {2, 3, 2,},
                {2, 3, 3,},
                {3, 1, 1,},
                {3, 1, 2,},
                {3, 1, 3,},
                {3, 2, 1,},
                {3, 2, 2,},
                {3, 2, 3,},
                {3, 3, 1,},
                {3, 3, 2,},
                {3, 3, 3,},
            };

            Dictionary<int, long> combinations = new();
            for (int index = 0; index < 27; index += 1)
            {
                int sum = outcomes[index, 0] + outcomes[index, 1] + outcomes[index, 2];
                if (combinations.TryGetValue(sum, out _))
                {
                    combinations[sum] += 1;
                }
                else
                {
                    combinations[sum] = 1;
                }
            }

            Dictionary<string, (long, long)> cache = new();
            (long, long) PlayGame((long place, long score) player1, (long place, long score) player2)
            {
                if (player2.score >= 21)
                {
                    return (0, 1);
                }

                string key = $"({player1.place}, {player1.score}), ({player2.place}, {player2.score})";
                if (cache.TryGetValue(key, out (long, long) result))
                {
                    return result;
                }

                result = (0, 0);
                foreach ((int combination, long count) in combinations)
                {
                    long newPlayer1Place = player1.place + combination;
                    newPlayer1Place = ((newPlayer1Place - 1) % 10) + 1;
                    
                    long newPlayer1Score = player1.score + newPlayer1Place;
                    
                    (long, long) win = PlayGame(player2, (newPlayer1Place, newPlayer1Score));
                    result.Item1 += win.Item2 * count;
                    result.Item2 += win.Item1 * count;
                }
                cache[key] = result;
                return result;
            }
            Console.WriteLine($"Done. Scores: {PlayGame((3, 0), (5, 0))}");
        }

        public class DiracDiceBoard
        {
            public DiracDiceBoard(params int[] starts)
            {
                this.Players = (from start in starts
                                select new DiracDicePlayer(start)).ToArray();
            }
            
            public int Dice
            {
                get;
                private set;
            }
            
            public DiracDicePlayer[] Players
            {
                get;
            }

            public void RollDice()
            {
                this.Dice = (this.Dice % 100) + 1;
            }
            
            public class DiracDicePlayer
            {
                public DiracDicePlayer(int start)
                {
                    this.Place = start;
                }
                
                public int Score
                {
                    get;
                    private set;
                }

                public int Place
                {
                    get;
                    private set;
                }

                public void UpdateScore(int rolls)
                {
                    if (this.Score is 30)
                    {
                        Console.WriteLine($"Score: {this.Score}, Place: {this.Place}, rolls: {rolls}");
                    }
                    this.Place += rolls;
                    if (this.Place > 10)
                    {
                        int mod = this.Place % 10;
                        this.Place = mod is 0 ? 10 : mod;
                    }
                    this.Score += this.Place;
                }
            }
        }
    }
}