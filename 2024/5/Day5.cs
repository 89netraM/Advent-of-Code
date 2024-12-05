using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2024;

[Day(5)]
public class Day5
{
    [Part(1)]
    public object Part1(string input)
    {
        var sections = input.Paragraphs();
        var orders = sections[0]
            .Lines()
            .Extract<(long, long)>(@"(\d+)\|(\d+)")
            .ToLookup(o => o.Item1, o => o.Item2);
        return sections[1]
            .Lines()
            .Select(l => l.Split(',').Select(long.Parse).ToArray())
            .Where(u =>
            {
                for (int i = 0; i < u.Length; i++)
                {
                    var next = orders[u[i]].ToHashSet();
                    if (next.Count == 0)
                        continue;
                    for (int j = 0; j < i; j++)
                    {
                        if (next.Contains(u[j]))
                            return false;
                    }
                }
                return true;
            })
            .Sum(u => u[u.Length / 2]);
    }

    [Part(2)]
    public object Part2(string input)
    {
        var sections = input.Paragraphs();
        var orders = sections[0]
            .Lines()
            .Extract<(long, long)>(@"(\d+)\|(\d+)")
            .ToLookup(o => o.Item1, o => o.Item2);
        var comparer = new UpdatesComparer(orders);
        return sections[1]
            .Lines()
            .Select(l => l.Split(',').Select(long.Parse).ToArray())
            .Where(u =>
            {
                for (int i = 0; i < u.Length; i++)
                {
                    var next = orders[u[i]].ToHashSet();
                    if (next.Count == 0)
                        continue;
                    for (int j = 0; j < i; j++)
                    {
                        if (next.Contains(u[j]))
                            return true;
                    }
                }
                return false;
            })
            .Select(u => u.Order(comparer).ToArray())
            .Sum(u => u[u.Length / 2]);
    }

    private class UpdatesComparer(ILookup<long, long> order) : IComparer<long>
    {
        public int Compare(long x, long y) =>
            order[x].Contains(y)
                ? -1
                : order[y].Contains(x)
                    ? 1
                    : 0;
    }
}
