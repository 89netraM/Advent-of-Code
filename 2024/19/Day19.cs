using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2024;

[Day(19)]
public class Day19
{
    [Part(1)]
    public object Part1(string input)
    {
        var ps = input.Paragraphs();
        var parts = ps[0]
            .Split(',', StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries);

        return ps[1]
            .Lines()
            .AsParallel()
            .Count(design =>
                BFS.Search(
                    "",
                    t =>
                        parts
                            .Select(p => t + p)
                            .Where(t => t.Length <= design.Length)
                            .Where(t => design.StartsWith(t)),
                    t => t == design,
                    out _
                )
            );
    }

    [Part(2)]
    public object Part2(string input)
    {
        var ps = input.Paragraphs();
        var parts = ps[0]
            .Split(',', StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries);
        var targets = ps[1].Lines();
        var cache = new Dictionary<string, long>();
        return targets.Sum(t => Count(t.AsSpan(), 0));

        long Count(ReadOnlySpan<char> target, long count)
        {
            var alternativeLookup = cache.GetAlternateLookup<ReadOnlySpan<char>>();
            if (alternativeLookup.TryGetValue(target, out var currentCost))
            {
                return count + currentCost;
            }
            currentCost = 0;
            foreach (var part in parts)
            {
                if (target.StartsWith(part))
                {
                    currentCost +=
                        target.Length == part.Length ? 1L : Count(target[part.Length..], count);
                }
            }
            alternativeLookup.TryAdd(target, currentCost);
            return currentCost;
        }
    }
}
