using System;
using System.Linq;
using AoC.Library;

namespace AoC.Year2024;

[Day(25)]
public class Day25
{
    [Part(1)]
    public object Part1(string input)
    {
        var groups = input.Paragraphs()
            .ToLookup(p => p[0], p => p.Lines().Transpose().Select(c => c.Count(c => c is '#')).ToArray());
        return groups['#'].Sum(l => groups['.'].Count(k => l.Zip(k, (l, k) => l + k).All(s => s <= 7)));
    }
}
