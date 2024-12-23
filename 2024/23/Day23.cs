using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2024;

[Day(23)]
public class Day23
{
    [Part(1)]
    public object Part1(string input)
    {
        var directConnections = input
            .Lines()
            .Extract<(string, string)>(@"(\w+)-(\w+)");
        var outgoingConnections =
            directConnections
            .Concat(directConnections.Select(p => (p.Item2, p.Item1)))
            .ToLookup(p => p.Item1, p => p.Item2)
            .ToDictionary(g => g.Key, g => g.ToHashSet());
        var lans = new HashSet<UOT<string>>();
        foreach (var (from, tos) in outgoingConnections)
        {
            if (!from.StartsWith('t'))
            {
                continue;
            }
            foreach (var a in tos)
            {
                foreach (var b in tos)
                {
                    if (a == b)
                    {
                        continue;
                    }
                    if (!outgoingConnections[a].Contains(from) || !outgoingConnections[a].Contains(b))
                    {
                        continue;
                    }
                    if (!outgoingConnections[b].Contains(from) || !outgoingConnections[b].Contains(a))
                    {
                        continue;
                    }
                    lans.Add(UOT.New(from, a, b));
                }
            }
        }
        return lans.Count;
    }

    [Part(2)]
    public object Part2(string input)
    {
        var directConnections = input
            .Lines()
            .Extract<(string, string)>(@"(\w+)-(\w+)");
        var outgoingConnections =
            directConnections
            .Concat(directConnections.Select(p => (p.Item2, p.Item1)))
            .ToLookup(p => p.Item1, p => p.Item2)
            .ToDictionary(g => g.Key, g => g.ToHashSet());
        var groups = new List<HashSet<string>>();
        var updated = true;
        while (updated)
        {
            updated = false;
            foreach (var (from, tos) in outgoingConnections)
            {
                var contains = false;
                foreach (var group in groups)
                {
                    if (group.Contains(from))
                    {
                        contains = true;
                        continue;
                    }
                    if (group.All(c => outgoingConnections[c].Contains(from) && tos.Contains(c)))
                    {
                        group.Add(from);
                        contains = true;
                        updated = true;
                    }
                }
                if (!contains)
                {
                    groups.Add([from]);
                }
            }
        }
        return string.Join(',', groups.MaxBy(g => g.Count).Order());
    }
}
