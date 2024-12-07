using System;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2024;

[Day(7)]
public class Day7
{
    [Part(1)]
    public object Part1(string input) =>
        input
            .Lines()
            .Extract<(long, string)>(@"(\d+): (.*)")
            .Select(p => (result: p.Item1, numbers: p.Item2.Words().Select(long.Parse).ToArray()))
            .Where(Possible)
            .Sum(p => p.result);

    private static bool Possible((long result, long[] numbers) pair)
    {
        var (result, numbers) = pair;
        var current = numbers[0];
        return IsPossible(result, current, numbers.AsSpan(1));

        static bool IsPossible(long result, long current, ReadOnlySpan<long> numbers)
        {
            if (result == current && numbers.Length == 0)
            {
                return true;
            }
            if (numbers.Length == 0)
            {
                return false;
            }
            if (current > result)
            {
                return false;
            }
            return IsPossible(result, current + numbers[0], numbers[1..])
                || IsPossible(result, current * numbers[0], numbers[1..]);
        }
    }

    [Part(2)]
    public object Part2(string input) =>
        input
            .Lines()
            .Extract<(long, string)>(@"(\d+): (.*)")
            .Select(p => (result: p.Item1, numbers: p.Item2.Words().Select(long.Parse).ToArray()))
            .Where(Possible2)
            .Sum(p => p.result);

    private static bool Possible2((long result, long[] numbers) pair)
    {
        var (result, numbers) = pair;
        var current = numbers[0];
        return IsPossible(result, current, numbers.AsSpan(1));

        static bool IsPossible(long result, long current, ReadOnlySpan<long> numbers)
        {
            if (result == current && numbers.Length == 0)
            {
                return true;
            }
            if (numbers.Length == 0)
            {
                return false;
            }
            if (current > result)
            {
                return false;
            }
            return IsPossible(result, current + numbers[0], numbers[1..])
                || IsPossible(result, current * numbers[0], numbers[1..])
                || IsPossible(
                    result,
                    long.Parse(current.ToString() + numbers[0].ToString()),
                    numbers[1..]
                );
        }
    }
}
