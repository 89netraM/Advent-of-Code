using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2024;

[Day(12)]
public class Day12
{
    [Part(1)]
    public object Part1(string input) => CalculatePriceForMap(input, CalculatePriceForPlotPart1);

    private static long CalculatePriceForMap(
        string input,
        Func<HashSet<Vector2>, long> priceForPlot
    )
    {
        var map = input.ToMap();
        var available = map.Keys.ToHashSet();
        var totalPrice = 0L;
        while (available.Count > 0)
        {
            var start = available.First();
            var plot = new HashSet<Vector2>() { start };
            BFS.Search(
                start,
                n =>
                    n.NeighborsVonNeumann()
                        .Where(n => map.TryGetValue(n, out var t) && map[start] == t),
                _ => false,
                out _,
                n => plot.Add(n)
            );
            available.ExceptWith(plot);

            totalPrice += priceForPlot(plot);
        }
        return totalPrice;
    }

    private static long CalculatePriceForPlotPart1(HashSet<Vector2> plot)
    {
        var area = plot.Count;
        var perimeter = plot.Sum(c => c.NeighborsVonNeumann().Count(n => !plot.Contains(n)));
        return area * perimeter;
    }

    [Part(2)]
    public object Part2(string input) => CalculatePriceForMap(input, CalculatePriceForPlotPart2);

    private static long CalculatePriceForPlotPart2(HashSet<Vector2> plot)
    {
        var area = plot.Count;
        var perimeterNodes = plot.Where(c => c.NeighborsVonNeumann().Any(n => !plot.Contains(n)))
            .ToHashSet();
        var sides = Vector2
            .Zero.NeighborsVonNeumann()
            .Sum(dir =>
            {
                var sides = plot.Where(p => !plot.Contains(p + dir))
                    .Select(p => new HashSet<Vector2> { p })
                    .ToList();
                for (var updated = true; updated; )
                {
                    updated = false;
                    for (var i = 0; i < sides.Count; i++)
                    {
                        for (var j = i + 1; j < sides.Count; j++)
                        {
                            if (
                                sides[i]
                                    .Any(a =>
                                        sides[j]
                                            .Any(b =>
                                                a + dir.Rotate(-1) == b || a + dir.Rotate(1) == b
                                            )
                                    )
                            )
                            {
                                sides[i].UnionWith(sides[j]);
                                sides.RemoveAt(j);
                                j--;
                                updated = true;
                            }
                        }
                    }
                }
                return sides.Count;
            });
        return area * sides;
    }
}
