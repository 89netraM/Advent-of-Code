using System;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2024;

[Day(18)]
public class Day18
{
    [Part(1)]
    public object Part1(string input)
    {
        var falls = input.Lines().Extract<Vector2>(@"(\d+),(\d+)").Take(1024).ToHashSet();
        BFS.Search(
            Vector2.Zero,
            n =>
                n.NeighborsVonNeumann()
                    .Where(n =>
                        !falls.Contains(n) && n.X is >= 0 and <= 70 && n.Y is >= 0 and <= 70
                    ),
            n => n.X is 70 && n.Y is 70,
            out var path
        );
        return path.Count();
    }

    [Part(2)]
    public object Part2(string input)
    {
        var falls = input.Lines().Extract<Vector2>(@"(\d+),(\d+)").ToArray();
        int min = 0;
        int max = falls.Length - 1;
        while (true)
        {
            int current = (min + max) / 2;
            if (min == current || current == max)
            {
                var blocking = falls[max];
                return $"{blocking.X},{blocking.Y}";
            }

            var blocks = falls.Take(current + 1).ToHashSet();
            var success = BFS.Search(
                Vector2.Zero,
                n =>
                    n.NeighborsVonNeumann()
                        .Where(n =>
                            !blocks.Contains(n) && n.X is >= 0 and <= 70 && n.Y is >= 0 and <= 70
                        ),
                n => n.X is 70 && n.Y is 70,
                out var path
            );
            if (success)
            {
                min = current;
            }
            else
            {
                max = current;
            }
        }
    }
}
