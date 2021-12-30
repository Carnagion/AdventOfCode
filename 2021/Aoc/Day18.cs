using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;

using Aoc.Extensions;

namespace Aoc
{
    public static class Day18
    {
        public static void Part1()
        {
            Queue<SnailfishNumber> numbers = new();
            while (true)
            {
                string? input = Console.ReadLine();
                if (input is null or "done")
                {
                    break;
                }

                SnailfishNumber number = new(input);
                numbers.Enqueue(number);
            }

            SnailfishNumber result = numbers.Dequeue();
            while (numbers.Any())
            {
                Console.WriteLine($"current result is {result}");
                result = SnailfishNumber.Add(result, numbers.Dequeue());
            }
            Console.WriteLine($"final result is {result}");
            Console.WriteLine($"magnitude is {result.Magnitude}");
        }

        public static void Part2()
        {
            List<SnailfishNumber> numbers = new();
            while (true)
            {
                string? input = Console.ReadLine();
                if (input is null or "done")
                {
                    break;
                }
                SnailfishNumber number = new(input);
                numbers.Add(number);
            }

            IEnumerable<int> magnitudes = from number in numbers
                                          from other in numbers
                                          where other != number
                                          let sum = SnailfishNumber.Add(new(number.ToString()), new(other.ToString()))
                                          let magnitude = sum.Magnitude
                                          orderby magnitude descending
                                          select magnitude;
            Console.WriteLine($"Done. Maximum: {magnitudes.Max()}");
        }

        public class SnailfishNumber
        {
            public SnailfishNumber(string expression, SnailfishNumber? parent = null)
            {
                this.Parent = parent;
                
                if (Int32.TryParse(expression, out int number))
                {
                    this.LiteralValue = number;
                    return;
                }
                
                string between = expression[1..^1];
                if (!(between.Contains('[') || between.Contains(']')))
                {
                    string[] values = between.Split(',');
                    (SnailfishNumber left, SnailfishNumber right) numbers = (new(values[0], this), new(values[1], this));
                    this.Numbers = numbers;
                }
                else
                {
                    int comma = SnailfishNumber.FindCommaIndex(between);
                    (SnailfishNumber left, SnailfishNumber right) numbers = (new(between[..comma], this), new(between[(comma + 1)..], this));
                    this.Numbers = numbers;
                }
            }

            public SnailfishNumber(SnailfishNumber left, SnailfishNumber right)
            {
                left.Parent = this;
                right.Parent = this;
                this.Numbers = (left, right);
            }

            public SnailfishNumber(SnailfishNumber copyFrom)
            {
                this.Parent = copyFrom.Parent;
                this.LiteralValue = copyFrom.LiteralValue;
                this.Numbers = copyFrom.Numbers;
            }
            
            public int? LiteralValue
            {
                get;
                private set;
            }

            public (SnailfishNumber left, SnailfishNumber right)? Numbers
            {
                get;
                private set;
            }

            public SnailfishNumber? Parent
            {
                get;
                private set;
            }

            public int Magnitude
            {
                get
                {
                    if (this.LiteralValue.HasValue)
                    {
                        return this.LiteralValue.Value;
                    }
                    return (3 * this.Numbers!.Value.left.Magnitude) + (2 * this.Numbers!.Value.right.Magnitude);
                }
            }

            public bool HasRecursiveValues
            {
                get
                {
                    if (this.LiteralValue.HasValue)
                    {
                        return false;
                    }
                    return this.Numbers!.Value.left.Numbers.HasValue || this.Numbers!.Value.right.Numbers.HasValue;
                }
            }

            public int NestedParentCount()
            {
                return this.Parent is null ? 0 : 1 + this.Parent.NestedParentCount();
            }

            public SnailfishNumber? Sibling(SearchSide side)
            {
                SnailfishNumber? parent = this.Parent;
                if (parent is null)
                {
                    return null;
                }

                (SnailfishNumber left, SnailfishNumber right) = parent.Numbers!.Value;
                switch (side)
                {
                    case SearchSide.Left:
                        return this == left ? null : left;
                    case SearchSide.Right:
                        return this == right ? null : right;
                    default:
                        throw new ArgumentOutOfRangeException(nameof(side), side, null);
                }
            }

            public SnailfishNumber? SidemostValue(SearchSide side)
            {
                if (this.LiteralValue.HasValue)
                {
                    return this;
                }
                return side switch
                {
                    SearchSide.Left => this.Numbers?.left.SidemostValue(side),
                    SearchSide.Right => this.Numbers?.right.SidemostValue(side),
                    _ => throw new ArgumentOutOfRangeException(nameof(side), side, null)
                };
            }

            public SnailfishNumber? ClosestLiteralValue(SearchSide side)
            {
                SnailfishNumber? sibling = this.Sibling(side);
                if (sibling is null)
                {
                    return this.Parent?.ClosestLiteralValue(side);
                }
                return sibling.SidemostValue(this.OppositeSideTo(side));
            }

            public IEnumerable<SnailfishNumber> GetAllNestedChildren()
            {
                if (!this.Numbers.HasValue)
                {
                    yield break;
                }

                yield return this.Numbers.Value.left;
                foreach (SnailfishNumber recursive in this.Numbers.Value.left.GetAllNestedChildren())
                {
                    yield return recursive;
                }
                yield return this.Numbers.Value.right;
                foreach (SnailfishNumber recursive in this.Numbers.Value.right.GetAllNestedChildren())
                {
                    yield return recursive;
                }
            }

            public IEnumerable<SnailfishNumber> GetAllNestedPairs()
            {
                return from child in this.GetAllNestedChildren()
                       where !child.LiteralValue.HasValue
                       select child;
            }

            public bool CanExplode([NotNullWhen(true)] out SnailfishNumber? leftmost)
            {
                leftmost = null;
                IEnumerable<SnailfishNumber> explodable = from pair in this.GetAllNestedPairs()
                                                          where !pair.HasRecursiveValues && (pair.NestedParentCount() >= 4)
                                                          select pair;
                if (!explodable.Any())
                {
                    return false;
                }
                string representation = this.ToString();
                leftmost = (from pair in explodable
                            orderby representation.IndexOf(pair.ToString(), StringComparison.Ordinal) + representation.IndexOf(pair.Parent!.ToString(), StringComparison.Ordinal)
                            select pair).First();
                return true;
            }

            public void Explode()
            {
                SnailfishNumber? regularLeft = this.ClosestLiteralValue(SearchSide.Left);
                if (regularLeft is not null)
                {
                    regularLeft.LiteralValue += this.Numbers!.Value.left.LiteralValue;
                }
                
                SnailfishNumber? regularRight = this.ClosestLiteralValue(SearchSide.Right);
                if (regularRight is not null)
                {
                    regularRight.LiteralValue += this.Numbers!.Value.right.LiteralValue;
                }

                (SnailfishNumber left, SnailfishNumber right) = this.Parent!.Numbers!.Value;
                if (this == left)
                {
                    this.Parent.Numbers = (new("0", this.Parent), right);
                }
                else if (this == right)
                {
                    this.Parent.Numbers = (left, new("0", this.Parent));
                }
            }

            public bool CanSplit([NotNullWhen(true)] out SnailfishNumber? leftmost)
            {
                leftmost = null;
                IEnumerable<SnailfishNumber> splittable = from number in this.GetAllNestedChildren()
                                                          where number.LiteralValue is >= 10
                                                          select number;
                if (!splittable.Any())
                {
                    return false;
                }
                string representation = this.ToString();
                leftmost = (from number in splittable
                            orderby representation.IndexOf(number.ToString(), StringComparison.Ordinal)
                            select number).First();
                return true;
            }

            public void Split()
            {
                int leftValue = (int)Math.Floor((double)this.LiteralValue!.Value / 2);
                int rightValue = (int)Math.Ceiling((double)this.LiteralValue!.Value / 2);

                (SnailfishNumber newLeft, SnailfishNumber newRight) = (new(leftValue.ToString(), this), new(rightValue.ToString(), this));
                this.LiteralValue = null;
                this.Numbers = (newLeft, newRight);
            }

            public void Reduce(ReduceAction? action = null, SnailfishNumber? number = null)
            {
                switch (action)
                {
                    case null:
                        if (this.CanExplode(out SnailfishNumber? toExplode))
                        {
                            this.Reduce(ReduceAction.Explode, toExplode);
                        }
                        else if (this.CanSplit(out SnailfishNumber? toSplit))
                        {
                            this.Reduce(ReduceAction.Split, toSplit);
                        }
                        break;
                    case ReduceAction.Explode:
                        number!.Explode();
                        if (this.CanExplode(out SnailfishNumber? toExplodeE))
                        {
                            this.Reduce(ReduceAction.Explode, toExplodeE);
                        }
                        else if (this.CanSplit(out SnailfishNumber? toSplitE))
                        {
                            this.Reduce(ReduceAction.Split, toSplitE);
                        }
                        break;
                    case ReduceAction.Split:
                        number!.Split();
                        if (this.CanExplode(out SnailfishNumber? toExplodeS))
                        {
                            this.Reduce(ReduceAction.Explode, toExplodeS);
                        }
                        else if (this.CanSplit(out SnailfishNumber? toSplitS))
                        {
                            this.Reduce(ReduceAction.Split, toSplitS);
                        }
                        break;
                    default:
                        throw new ArgumentOutOfRangeException(nameof(action), action, null);
                }
            }

            public static SnailfishNumber Add(SnailfishNumber left, SnailfishNumber right)
            {
                SnailfishNumber result = new(left, right);
                result.Reduce();
                return result;
            }

            public static int FindCommaIndex(string expression)
            {
                if (expression[1] is ',')
                {
                    return 1;
                }
            
                Stack<char> brackets = new();
                for (int index = 0; index < expression.Length; index += 1)
                {
                    if (expression[index] is not ('[' or ']'))
                    {
                        continue;
                    }
                
                    brackets.TryPeek(out char peek);
                    if (peek.OppositeChar() == expression[index])
                    {
                        brackets.Pop();
                        if (!brackets.Any())
                        {
                            return index + 1;
                        }
                    }
                    else
                    {
                        brackets.Push(expression[index]);
                    }
                }
                return -1;
            }

            public SearchSide OppositeSideTo(SearchSide side)
            {
                return side switch
                {
                    SearchSide.Left => SearchSide.Right,
                    SearchSide.Right => SearchSide.Left,
                    _ => throw new ArgumentOutOfRangeException(nameof(side), side, null),
                };
            }

            public override string ToString()
            {
                return this.LiteralValue.HasValue ? this.LiteralValue.Value.ToString() : $"[{this.Numbers?.left},{this.Numbers?.right}]";
            }

            public enum SearchSide
            {
                Left,
                Right,
            }

            public enum ReduceAction
            {
                Explode,
                Split,
            }
        }
    }
}