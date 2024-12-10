using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using AoC.Library;

namespace AoC.Year2024;

[Day(10)]
public class Day10
{
    [Part(1)]
    public object Part1(string input)
    {
        var map = input.ToMap().ToDictionary(kvp => kvp.Key, kvp => (long)(kvp.Value - '0'));
        return map.Where(kvp => kvp.Value is 0)
            .Select(kvp => kvp.Key)
            .Sum(s =>
            {
                long count = 0;
                BFS.Search(
                    s,
                    f =>
                        f.NeighborsVonNeumann()
                            .Where(n => map.ContainsKey(n) && map[f] + 1 == map[n]),
                    _ => false,
                    out _,
                    n =>
                    {
                        if (map[n] is 9)
                        {
                            count++;
                        }
                    }
                );
                return count;
            });
    }

    [Part(2)]
    public object Part2(string input)
    {
        var map = input.ToMap().ToDictionary(kvp => kvp.Key, kvp => (long)(kvp.Value - '0'));
        return map.Where(kvp => kvp.Value is 0)
            .Select(kvp => ImmutableList.Create(kvp.Key))
            .Sum(s =>
            {
                long count = 0;
                BFS.Search(
                    s,
                    f =>
                        f[^1]
                            .NeighborsVonNeumann()
                            .Where(n => map.ContainsKey(n) && map[f[^1]] + 1 == map[n])
                            .Select(n => f.Add(n)),
                    _ => false,
                    out _,
                    path =>
                    {
                        if (map[path[^1]] is 9)
                        {
                            count++;
                        }
                    }
                );
                return count;
            });
    }
}
