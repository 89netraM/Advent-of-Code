using System.Collections.Generic;
using System.Data;
using System.Linq;
using AoC.Library;
using static AoC.Library.Functional;

namespace AoC.Year2024;

[Day(11)]
public class Day11
{
    [Part(1)]
    public object Part1(string input) => CountStonesAfter(input, 25);

    [Part(2)]
    public object Part2(string input) => CountStonesAfter(input, 75);

    private static long CountStonesAfter(string input, int blinkCount)
    {
        var stones = input.Words().Select(long.Parse).ToDictionary(Id, _ => 1L);
        var stones2 = new Dictionary<long, long>();
        for (var i = 0; i < blinkCount; i++)
        {
            foreach (var (stone, count) in stones)
            {
                if (stone is 0)
                {
                    stones2.Increase(1, count);
                }
                else if (stone.ToString() is string s && s.Length % 2 is 0)
                {
                    stones2.Increase(long.Parse(s[..(s.Length / 2)]), count);
                    stones2.Increase(long.Parse(s[(s.Length / 2)..]), count);
                }
                else
                {
                    stones2.Increase(stone * 2024, count);
                }
            }

            (stones, stones2) = (stones2, stones);
            stones2.Clear();
        }
        return stones.Sum(kvp => kvp.Value);
    }
}
