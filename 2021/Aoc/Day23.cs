using System;

namespace Aoc
{
    public static class Day23
    {
        public static void Part1()
        {
            Console.WriteLine("Solved manually :)");
        }

        public static void Part2()
        {
            Console.WriteLine("Solved manually :)");
        }
        
        /*public static int[] HallwayDoorPositions
        {
            get;
        } = {2, 4, 6, 8,};

        public static Dictionary<char, int> TargetPositions
        {
            get;
        } = new()
        {
            {'A', 2},
            {'B', 4},
            {'C', 6},
            {'D', 8},
        };

        public static Dictionary<char, int> AmphipodMoveCost
        {
            get;
        } = new()
        {
            {'A', 1},
            {'B', 10},
            {'C', 100},
            {'D', 1000},
        };

        public static bool CanReachPosition(this string[] board, int from, int to)
        {
            for (int index = Math.Min(from, to); index < Math.Max(from, to); index += 1)
            {
                if (Day23.HallwayDoorPositions.Contains(index) || (index == from))
                {
                    continue;
                }
                if (board[index] is not ".")
                {
                    return false;
                }
            }
            return true;
        }

        public static bool WillAcceptRoom(this char amphipod, string room)
        {
            return room.Length == room.Count(character => character is '.') + room.Count(character => character == amphipod);
        }

        public static char GetOutermostAmphipod(this string room)
        {
            return room.FirstOrDefault(character => character is not '.');
        }

        public static IEnumerable<int> PossibleLocationIndexes(this string[] board, int position)
        {
            string room = board[position];
            
            if (!Day23.HallwayDoorPositions.Contains(position))
            {
                char amphipod = room[0];
                if (amphipod is '.')
                {
                    yield break;
                }
                int destination = Day23.TargetPositions[amphipod];
                if (board.CanReachPosition(position, destination) && amphipod.WillAcceptRoom(board[destination]))
                {
                    yield return destination;
                    yield break;
                }
                yield break;
            }

            char outermost = room.GetOutermostAmphipod();
            if ((position == Day23.TargetPositions[outermost]) && outermost.WillAcceptRoom(board[position]))
            {
                yield break;
            }

            for (byte index = 0; index < board.Length; index += 1)
            {
                if (index == position)
                {
                    continue;
                }
                if (Day23.HallwayDoorPositions.Contains(index) && (Day23.TargetPositions[outermost] != index))
                {
                    continue;
                }
                if (Day23.TargetPositions[outermost] == index && !outermost.WillAcceptRoom(board[index]))
                {
                    continue;
                }
                if (board.CanReachPosition(position, index))
                {
                    yield return index;
                }
            }
        }

        public static (string room, int count) MoveToRoom(this char amphipod, string room)
        {
            char[] array = room.ToArray();
            int count = array.Count(character => character is '.');
            array[count - 1] = amphipod;
            return (new(array), count);
        }

        public static (string[] board, int count) MoveBoard(this string[] board, int from, int to)
        {
            string[] result = board[..];
            int count = 0;
            string room = board[from];
            char outermost = room.GetOutermostAmphipod();
            
            if (room.Length is 1)
            {
                result[from] = ".";
            }
            else
            {
                string newRoom = "";
                bool found = false;
                foreach (char character in room)
                {
                    if (character is '.')
                    {
                        count += 1;
                        newRoom += character;
                    }
                    else if (!found)
                    {
                        newRoom += '.';
                        count += 1;
                        found = true;
                    }
                    else
                    {
                        newRoom += character;
                    }
                }
                result[from] = newRoom;
            }
            count += Math.Abs(from - to);

            if (board[to].Length is 1)
            {
                result[to] = outermost.ToString();
                return (result, count * Day23.AmphipodMoveCost[outermost]);
            }
            (string addedRoom, int addedCount) = outermost.MoveToRoom(board[to]);
            result[to] = addedRoom;
            count += addedCount;
            return (result, count * Day23.AmphipodMoveCost[outermost]);
        }

        public static Dictionary<string[], int> GetAllBoardStates(this string[] board)
        {
            Dictionary<string[], int> states = new() {{board, 0},};

            Stack<string[]> stack = new();
            stack.Push(board);

            while (stack.Any())
            {
                string[] current = stack.Pop();
                for (int index = 0; index < current.Length; index += 1)
                {
                    string room = current[index];
                    if (room.GetOutermostAmphipod() is default(char))
                    {
                        continue;
                    }
                    IEnumerable<int> possibleDestinations = board.PossibleLocationIndexes(index);
                    foreach (int destination in possibleDestinations)
                    {
                        (string[] newBoard, int addedCost) = board.MoveBoard(index, destination);
                        int newCost = states[board] + addedCost;
                        int cost = states.GetValueOrDefault(newBoard, Int32.MaxValue);
                        if (newCost >= cost)
                        {
                            continue;
                        }
                        states[newBoard] = newCost;
                        stack.Push(newBoard);
                    }
                }
            }
            
            return states;
        }*/
    }
}