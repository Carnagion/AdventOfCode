using System;
using System.Collections.Generic;
using System.Linq;

using Aoc.Extensions;

namespace Aoc
{
    public static class Day4
    {
        public static void Part1()
        {
            string input = Console.ReadLine();
            IEnumerable<int> calledNumbers = from string number in input!.Split(",", StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
                                             select Int32.Parse(number);

            List<BingoBoard> boards = new();
            boards.Add(new());

            int index = 0;
            input = Console.ReadLine();
            while (input is not null)
            {
                if (input is "done")
                {
                    break;
                }

                string[] split = input!.Split(" ", StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
                if (split.Length is 0 || !Int32.TryParse(split[0], out int _))
                {
                    boards.Add(new());
                    index += 1;
                }
                else
                {
                    IEnumerable<int> row = from string number in split
                                           select Int32.Parse(number);
                    boards[index].AddRow(row);
                }

                input = Console.ReadLine();
            }

            BingoBoard win = null;
            foreach (int calledNumber in calledNumbers)
            {
                foreach (BingoBoard board in boards)
                {
                    if (board.CheckNumber(calledNumber) && board.CheckWin())
                    {
                        win = board;
                        break;
                    }
                }
                if (win is not null)
                {
                    int sum = 0;
                    foreach (int unmarked in win.UnmarkedNumbers)
                    {
                        sum += unmarked;
                    }
                    Console.WriteLine($"Done. Unmarked sum: {sum}. Winning number: {calledNumber}");
                    break;
                }
            }
        }

        public static void Part2()
        {
            string input = Console.ReadLine();
            IEnumerable<int> calledNumbers = from string number in input!.Split(",", StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
                                             select Int32.Parse(number);

            List<BingoBoard> boards = new();
            boards.Add(new());

            int index = 0;
            input = Console.ReadLine();
            while (input is not null)
            {
                if (input is "done")
                {
                    break;
                }

                string[] split = input!.Split(" ", StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
                if (split.Length is 0 || !Int32.TryParse(split[0], out int _))
                {
                    boards.Add(new());
                    index += 1;
                }
                else
                {
                    IEnumerable<int> row = from string number in split
                                           select Int32.Parse(number);
                    boards[index].AddRow(row);
                }

                input = Console.ReadLine();
            }

            foreach (int calledNumber in calledNumbers)
            {
                if (boards.Count is 1)
                {
                    if (boards[0].CheckNumber(calledNumber) && boards[0].CheckWin())
                    {
                        int sum = 0;
                        foreach (int unmarked in boards[0].UnmarkedNumbers)
                        {
                            sum += unmarked;
                        }
                        Console.WriteLine($"Done. Unmarked sum: {sum}. Winning number: {calledNumber}");
                        break;
                    }
                    continue;
                }
                boards.RemoveAll(board => board.CheckNumber(calledNumber) && board.CheckWin());
            }
        }

        public class BingoBoard
        {
            private List<List<int>> board = new();

            private List<int> wins = new();

            public IEnumerable<IEnumerable<int>> Rows
            {
                get
                {
                    return from List<int> row in this.board
                           select row;
                }
            }

            public IEnumerable<IEnumerable<int>> Columns
            {
                get
                {
                    for (int index = 0; index < this.board.Count; index += 1)
                    {
                        yield return from List<int> row in this.board
                                     select row[index];
                    }
                }
            }

            public IEnumerable<int> UnmarkedNumbers
            {
                get
                {
                    return from List<int> row in this.board
                           from int number in row
                           where !this.wins.Contains(number)
                           select number;
                }
            }

            public void AddRow(IEnumerable<int> row)
            {
                this.board.Add(new(row));
            }

            public void AddColumn(IEnumerable<int> column)
            {
                if (column.Count() != this.board.Count)
                {
                    return;
                }

                int index = 0;
                foreach (int number in column)
                {
                    this.board[index].Insert(0, index);
                    index += 1;
                }
            }

            public bool CheckNumber(int number)
            {
                if (!this.UnmarkedNumbers.Contains(number))
                {
                    return false;
                }
                this.wins.Add(number);
                return true;
            }

            public bool CheckWin()
            {
                if (this.wins.Count < this.board.Count)
                {
                    return false;
                }
                return this.Rows.Any(row => this.wins.ContainsAll(row)) || this.Columns.Any(column => this.wins.ContainsAll(column));
            }
        }
    }
}