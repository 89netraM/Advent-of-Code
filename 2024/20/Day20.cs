using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2024;

[Day(20)]
public class Day20
{
    [Part(1)]
    public object Part1(string input) => Solve(input, 2);

    [Part(2)]
    public object Part2(string input) => Solve(input, 20);

    private static long Solve(string input, long cheatDistance)
    {
        var map = input.ToMap();
        var start = map.Single(kvp => kvp.Value is 'S').Key;
        var end = map.Single(kvp => kvp.Value is 'E').Key;
        var track = map.Where(kvp => kvp.Value is not '#').Select(kvp => kvp.Key).ToHashSet();
        var walls = map.Where(kvp => kvp.Value is '#').Select(kvp => kvp.Key).ToHashSet();

        var distanceFromStart = DistanceTo(track, start);
        var distanceToEnd = DistanceTo(track, end);

        var noCheatDistance = distanceFromStart[end];

        return distanceFromStart
            .AsParallel()
            .Sum(hackStart =>
                hackStart
                    .Key.NeighborsVonNeumann(cheatDistance)
                    .Where(distanceToEnd.ContainsKey)
                    .Select(hackEnd =>
                        hackStart.Value
                        + hackStart.Key.ManhattanDistance(hackEnd)
                        + distanceToEnd[hackEnd]
                    )
                    .Count(cheatDistance => noCheatDistance - cheatDistance >= 100)
            );
    }

    private static Dictionary<Vector2, long> DistanceTo(
        HashSet<Vector2> validPositions,
        Vector2 target,
        long maxDistance = long.MaxValue
    )
    {
        var distanceTo = new Dictionary<Vector2, long> { [target] = 0 };
        var toVisit = new Queue<Vector2>();
        toVisit.Enqueue(target);
        while (toVisit.TryDequeue(out var position))
        {
            var currentDistance = distanceTo[position];
            foreach (var neighbor in position.NeighborsVonNeumann())
            {
                var neighborDistance = currentDistance + 1;
                if (
                    neighborDistance < maxDistance
                    && validPositions.Contains(neighbor)
                    && !distanceTo.ContainsKey(neighbor)
                )
                {
                    distanceTo[neighbor] = neighborDistance;
                    toVisit.Enqueue(neighbor);
                }
            }
        }
        return distanceTo;
    }
}
