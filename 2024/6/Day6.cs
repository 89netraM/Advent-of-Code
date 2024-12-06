using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using AoC.Library;

namespace AoC.Year2024;

[Day(6)]
public class Day6
{
    [Part(1)]
    public object Part1(string input)
    {
        var map = input.ToMap();
        var min = map.Keys.Min();
        var max = map.Keys.Max();
        var position = map.First(kvp => kvp.Value is '^').Key;
        var direction = Vector2.Up;
        HashSet<Vector2> positions = [position];
        while (
            min.X <= position.X && min.Y <= position.Y && position.X <= max.X && position.Y <= max.Y
        )
        {
            if (!map.TryGetValue(position + direction, out var next) || next is not '#')
            {
                position += direction;
                positions.Add(position);
            }
            else
            {
                direction = direction.Rotate(1);
            }
        }
        return positions.Count - 1;
    }

    [Part(2)]
    public object Part2(string input)
    {
        var map = input.ToMap().ToImmutableDictionary();
        var min = map.Keys.Min();
        var max = map.Keys.Max();
        var position = map.First(kvp => kvp.Value is '^').Key;
        var direction = Vector2.Up;
        return map.Where(kvp => kvp.Value is '.')
            .AsParallel()
            .Count(kvp => IsLoop(map.SetItem(kvp.Key, '#'), min, max, position, direction));
    }

    private static bool IsLoop(
        IReadOnlyDictionary<Vector2, char> map,
        Vector2 min,
        Vector2 max,
        Vector2 position,
        Vector2 direction
    )
    {
        HashSet<(Vector2, Vector2)> positions = [(position, direction)];
        while (
            min.X <= position.X && min.Y <= position.Y && position.X <= max.X && position.Y <= max.Y
        )
        {
            if (!map.TryGetValue(position + direction, out var next) || next is not '#')
            {
                position += direction;
                if (!positions.Add((position, direction)))
                {
                    return true;
                }
            }
            else
            {
                direction = direction.Rotate(1);
            }
        }
        return false;
    }
}
