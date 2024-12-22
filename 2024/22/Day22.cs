using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2024;

[Day(22)]
public class Day22
{
    [Part(1)]
    public object Part1(string input) =>
        input
            .Lines()
            .Select(long.Parse)
            .Sum(secret =>
            {
                for (var i = 0; i < 2000; i++)
                {
                    secret = (secret ^ (secret << 6)) & 0xFFFFFF;
                    secret = (secret ^ (secret >> 5)) & 0xFFFFFF;
                    secret = (secret ^ (secret << 11)) & 0xFFFFFF;
                }
                return secret;
            });

    [Part(2)]
    public object Part2(string input) =>
        input
            .Lines()
            .AsParallel()
            .Select(long.Parse)
            .SelectMany(s =>
                GenerateBananaCounts(s)
                    .Zip(GenerateBananaCounts(s).Skip(1), (l, h) => (bananas: h, change: h - l))
                    .Window4()
                    .GroupBy(GetChanges)
                    .Select(g => (changes: g.Key, g.First().Item4.bananas))
            )
            .GroupBy(g => g.changes)
            .Max(g => g.Sum(g => g.bananas));

    private static IEnumerable<long> GenerateBananaCounts(long secret)
    {
        yield return secret % 10L;
        for (var i = 0; i < 2000; i++)
        {
            secret = (secret ^ (secret << 6)) & 0xFFFFFF;
            secret = (secret ^ (secret >> 5)) & 0xFFFFFF;
            secret = (secret ^ (secret << 11)) & 0xFFFFFF;
            yield return secret % 10L;
        }
    }

    private static (long, long, long, long) GetChanges(
        (
            (long bananas, long change),
            (long bananas, long change),
            (long bananas, long change),
            (long bananas, long change)
        ) window
    ) => (window.Item1.change, window.Item2.change, window.Item3.change, window.Item4.change);
}
