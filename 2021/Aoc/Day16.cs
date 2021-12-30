using System;
using System.Collections.Generic;
using System.Linq;

using Aoc.Extensions;

namespace Aoc
{
    public static class Day16
    {
        public static void Part1()
        {
            string input = Console.ReadLine();
            string binary = input.ToBinaryString();

            Packet packet = new(binary);
            long versionSum = packet.VersionSum();
            Console.WriteLine($"Done. Version sum: {versionSum}");
        }

        public static void Part2()
        {
            string input = Console.ReadLine();
            string binary = input.ToBinaryString();

            Packet packet = new(binary);
            Console.WriteLine($"Done. Value: {packet.Value}");
        }

        public record Packet
        {
            public Packet(string binary)
            {
                string version = binary[..3];
                string typeId = binary[3..6];
                string data = binary[6..];

                this.Version = Convert.ToInt32(version, 2);
                this.TypeId = Convert.ToInt32(typeId, 2);

                if (this.TypeId is 4)
                {
                    string literal = "";
                    int index = 0;
                    while (true)
                    {
                        if (index > data.Length)
                        {
                            break;
                        }
                        
                        literal += data[(index + 1)..(index + 5)];
                        if (data[index] is '0')
                        {
                            break;
                        }
                        index += 5;
                    }
                    this.LiteralValue = Convert.ToInt64(literal, 2);
                    this.Length = 6 + literal.Length + (literal.Length / 4);
                    return;
                }
                
                this.LengthTypeId = data[0] is '1';
                if (this.LengthTypeId)
                {
                    int subPacketCount = Convert.ToInt32(data[1..12], 2);
                    string subPacketData = data[12..];

                    int length = 18;
                    int index = 0;
                    List<Packet> subPackets = new();
                    for (int count = 0; count < subPacketCount; count += 1)
                    {
                        subPackets.Add(new(subPacketData[index..]));
                        Packet last = subPackets[^1];
                        index += last.Length;
                        length += last.Length;
                    }

                    this.SubPackets = from packet in subPackets
                                      select packet;

                    this.Length = length;
                }
                else
                {
                    int subPacketLength = Convert.ToInt32(data[1..16], 2);
                    string subPacketData = data[16..(16 + subPacketLength)];

                    int index = 0;
                    List<Packet> subPackets = new();
                    while (index < subPacketData.Length)
                    {
                        subPackets.Add(new(subPacketData[index..]));
                        index += subPackets[^1].Length;
                    }

                    this.SubPackets = from packet in subPackets
                                      select packet;

                    this.Length = 22 + subPacketLength;
                }
            }

            public int Length
            {
                get;
            }
            
            public int Version
            {
                get;
            }

            public int TypeId
            {
                get;
            }

            public long LiteralValue
            {
                get;
            }

            public bool LengthTypeId
            {
                get;
            }

            public IEnumerable<Packet> SubPackets
            {
                get;
            } = Enumerable.Empty<Packet>();

            public long Value
            {
                get
                {
                    switch (this.LiteralValue)
                    {
                        case 0:
                            switch (this.TypeId)
                            {
                                case 0:
                                    return this.SubPackets.Sum(subPacket => subPacket.Value);
                                case 1:
                                    return this.SubPackets.Aggregate(1L, (product, next) => product * next.Value);
                                case 2:
                                    return this.SubPackets.Min(subPacket => subPacket.Value);
                                case 3:
                                    return this.SubPackets.Max(subPacket => subPacket.Value);
                                case 5:
                                    List<Packet> subPacketsGreater = this.SubPackets.ToList();
                                    return subPacketsGreater[0].Value > subPacketsGreater[1].Value ? 1 : 0;
                                case 6:
                                    List<Packet> subPacketsLesser = this.SubPackets.ToList();
                                    return subPacketsLesser[0].Value < subPacketsLesser[1].Value ? 1 : 0;
                                case 7:
                                    List<Packet> subPacketsEqual = this.SubPackets.ToList();
                                    return subPacketsEqual[0].Value == subPacketsEqual[1].Value ? 1 : 0;
                                default:
                                    return this.LiteralValue;
                            }
                        default:
                            return this.LiteralValue;
                    }
                }
            }

            public long VersionSum()
            {
                if (!this.SubPackets.Any())
                {
                    return this.Version;
                }
                return this.Version + this.SubPackets.Sum(subPacket => subPacket.VersionSum());
            }

            public override string ToString()
            {
                string result = $"Length: {this.Length}\nVersion: {this.Version}\nType ID: {this.TypeId}\n";
                result += this.LiteralValue switch
                {
                    0 => $"Length type ID: {(this.LengthTypeId ? '1' : '0')}\n{this.SubPackets.Aggregate("", (current, subPacket) => $"{current}\n\n{subPacket}")}\n\n\n",
                    _ => $"Literal value: {this.LiteralValue}",
                };
                return result;
            }
        }
    }
}