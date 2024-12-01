using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2024;

[Day(1)]
public class Day1
{
    [Part(1)]
    public object Part1(string input)
    {
        var columns = input
            .Lines()
            .Select(l =>
                l.Split(" ", StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
                    .Select(int.Parse)
            )
            .Transpose()
            .Select(c => c.Order())
            .ToArray();
        return columns[0].Zip(columns[1], (l, r) => int.Abs(l - r)).Sum();
    }

    [Part(2)]
    public object Part2(string input)
    {
        var columns = input
            .Lines()
            .Select(l =>
                l.Split(" ", StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
                    .Select(int.Parse)
            )
            .Transpose()
            .ToArray();
        var right = columns[1].ToCounter();
        return columns[0].Sum(l => l * right.GetValueOrDefault(l));
    }
}
