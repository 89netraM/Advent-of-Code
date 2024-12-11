// --- Day 11: Plutonian Pebbles ---
// The ancient civilization on Pluto was known for its ability to manipulate spacetime, and while the Historians explore their infinite corridors, you've noticed a strange set of physics-defying stones.
// At first glance, they seem like normal stones: they're arranged in a perfectly straight line, and each stone has a number engraved on it.
// The strange part is that every time you blink, the stones change.
// Sometimes, the number engraved on a stone changes. Other times, a stone might split in two, causing all the other stones to shift over a bit to make room in their perfectly straight line.
// As you observe them for a while, you find that the stones have a consistent behavior. Every time you blink, the stones each simultaneously change according to the first applicable rule in this list:
// If the stone is engraved with the number 0, it is replaced by a stone engraved with the number 1.
// If the stone is engraved with a number that has an even number of digits, it is replaced by two stones. The left half of the digits are engraved on the new left stone, and the right half of the digits are engraved on the new right stone. (The new numbers don't keep extra leading zeroes: 1000 would become stones 10 and 0.)
// If none of the other rules apply, the stone is replaced by a new stone; the old stone's number multiplied by 2024 is engraved on the new stone.
// No matter how the stones change, their order is preserved, and they stay on their perfectly straight line.
// How will the stones evolve if you keep blinking at them? You take a note of the number engraved on each stone in the line (your puzzle input).
// If you have an arrangement of five stones engraved with the numbers 0 1 10 99 999 and you blink once, the stones transform as follows:
// The first stone, 0, becomes a stone marked 1.
// The second stone, 1, is multiplied by 2024 to become 2024.
// The third stone, 10, is split into a stone marked 1 followed by a stone marked 0.
// The fourth stone, 99, is split into two stones marked 9.
// The fifth stone, 999, is replaced by a stone marked 2021976.
// So, after blinking once, your five stones would become an arrangement of seven stones engraved with the numbers 1 2024 1 0 9 9 2021976.
// Here is a longer example:
// Initial arrangement:
// 125 17
//
// After 1 blink:
// 253000 1 7
//
// After 2 blinks:
// 253 0 2024 14168
//
// After 3 blinks:
// 512072 1 20 24 28676032
//
// After 4 blinks:
// 512 72 2024 2 0 2 4 2867 6032
//
// After 5 blinks:
// 1036288 7 2 20 24 4048 1 4048 8096 28 67 60 32
//
// After 6 blinks:
// 2097446912 14168 4048 2 0 2 4 40 48 2024 40 48 80 96 2 8 6 7 6 0 3 2
// In this example, after blinking six times, you would have 22 stones. After blinking 25 times, you would have 55312 stones!
// Consider the arrangement of stones in front of you. How many stones will you have after blinking 25 times?
// To begin, get your puzzle input.
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Data;
using System.Dynamic;
using System.Linq;
using System.Text.RegularExpressions;
using AoC.Library;
using RegExtract;
using static AoC.Library.Functional;

namespace AoC.Year2024;

[Day(11)]
public class Day11
{
    [Part(1)]
    public object Part1(string input)
    {
        var stones = input.Words().Select(long.Parse).ToList();
        var stones2 = new List<long>();
        for (var i = 0; i < 25; i++)
        {
            foreach (var stone in stones)
            {
                if (stone is 0)
                {
                    stones2.Add(1);
                }
                else if (stone.ToString() is string s && s.Length % 2 is 0)
                {
                    stones2.Add(long.Parse(s[..(s.Length / 2)]));
                    stones2.Add(long.Parse(s[(s.Length / 2)..]));
                }
                else
                {
                    stones2.Add(stone * 2024);
                }
            }

            (stones, stones2) = (stones2, stones);
            stones2.Clear();
        }
        return stones.Count;
    }

    [Part(2)]
    public object Part2(string input)
    {
        var stones = new Stack<(long steps, long stone)>(
            input.Words().Select(s => (75L, long.Parse(s)))
        );
        var results = new Stack<(long steps, long stones)>();
        var memo = new Dictionary<(long steps, long stone), long>();
        long count = 0;
        while (stones.TryPop(out var pair))
        {
            var (steps, stone) = pair;

            if (steps is 0)
            {
                count++;
            }
            else if (stone is 0)
            {
                stones.Push((steps - 1, 1));
            }
            else if (stone.ToString() is string stoneString && stoneString.Length % 2 is 0)
            {
                stones.Push((steps - 1, long.Parse(stoneString[..(stoneString.Length / 2)])));
                stones.Push((steps - 1, long.Parse(stoneString[(stoneString.Length / 2)..])));
            }
            else
            {
                stones.Push((steps - 1, stone * 2024));
            }
        }
        return count;
    }

    public static long CountAfterSteps((long stone, long steps) pair)
    {
        var (stone, steps) = pair;
        if (steps is 0)
        {
            return 1;
        }

        if (stone is 0)
        {
            stone = 1;
            steps--;
        }

        var stoneString = stone.ToString();
        for (; stoneString.Length % 2 is not 0; stoneString = stone.ToString())
        {
            stone *= 2024;
            steps--;
            if (steps is 0)
            {
                return 1;
            }
        }

        var leftStone = long.Parse(stoneString[..(stoneString.Length / 2)]);
        var rightStone = long.Parse(stoneString[(stoneString.Length / 2)..]);
        steps--;

        return CountAfterStepsMemo((leftStone, steps)) + CountAfterStepsMemo((rightStone, steps));
    }

    private static readonly Func<(long, long), long> CountAfterStepsMemo = Memoize<
        (long, long),
        long
    >(CountAfterSteps);
}
