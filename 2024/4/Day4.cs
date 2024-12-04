using System.Linq;
using AoC.Library;

namespace AoC.Year2024;

[Day(4)]
public class Day4
{
    [Part(1)]
    public object Part1(string input)
    {
        var map = input.ToMap();
        var min = map.Keys.Min();
        var max = map.Keys.Max();
        long count = 0;
        for (var y = min.Y; y <= max.Y; y++)
        for (var x = min.X; x <= max.X; x++)
        {
            // Right
            if (
                x + 3 <= max.X
                && map[new(x, y)] is 'X'
                && map[new(x + 1, y)] is 'M'
                && map[new(x + 2, y)] is 'A'
                && map[new(x + 3, y)] is 'S'
            )
            {
                count++;
            }
            // Left
            if (
                x - 3 >= min.X
                && map[new(x, y)] is 'X'
                && map[new(x - 1, y)] is 'M'
                && map[new(x - 2, y)] is 'A'
                && map[new(x - 3, y)] is 'S'
            )
            {
                count++;
            }
            // Down right
            if (
                x + 3 <= max.X
                && y + 3 <= max.Y
                && map[new(x, y)] is 'X'
                && map[new(x + 1, y + 1)] is 'M'
                && map[new(x + 2, y + 2)] is 'A'
                && map[new(x + 3, y + 3)] is 'S'
            )
            {
                count++;
            }
            // Down left
            if (
                x - 3 >= min.X
                && y + 3 <= max.Y
                && map[new(x, y)] is 'X'
                && map[new(x - 1, y + 1)] is 'M'
                && map[new(x - 2, y + 2)] is 'A'
                && map[new(x - 3, y + 3)] is 'S'
            )
            {
                count++;
            }
            // Up right
            if (
                x + 3 <= max.X
                && y - 3 >= min.Y
                && map[new(x, y)] is 'X'
                && map[new(x + 1, y - 1)] is 'M'
                && map[new(x + 2, y - 2)] is 'A'
                && map[new(x + 3, y - 3)] is 'S'
            )
            {
                count++;
            }
            // Up left
            if (
                x - 3 >= min.X
                && y - 3 >= min.Y
                && map[new(x, y)] is 'X'
                && map[new(x - 1, y - 1)] is 'M'
                && map[new(x - 2, y - 2)] is 'A'
                && map[new(x - 3, y - 3)] is 'S'
            )
            {
                count++;
            }
            // Down
            if (
                y + 3 <= max.Y
                && map[new(x, y)] is 'X'
                && map[new(x, y + 1)] is 'M'
                && map[new(x, y + 2)] is 'A'
                && map[new(x, y + 3)] is 'S'
            )
            {
                count++;
            }
            // Up
            if (
                y - 3 >= min.Y
                && map[new(x, y)] is 'X'
                && map[new(x, y - 1)] is 'M'
                && map[new(x, y - 2)] is 'A'
                && map[new(x, y - 3)] is 'S'
            )
            {
                count++;
            }
        }
        return count;
    }

    [Part(2)]
    public object Part2(string input)
    {
        var map = input.ToMap();
        var min = map.Keys.Min();
        var max = map.Keys.Max();
        long count = 0;
        for (var y = min.Y; y <= max.Y - 2; y++)
        for (var x = min.X; x <= max.X - 2; x++)
        {
            if (
                (map[new(x, y)], map[new(x + 1, y + 1)], map[new(x + 2, y + 2)])
                    is
                        ('M', 'A', 'S')
                        or
                        ('S', 'A', 'M')
                && (map[new(x, y + 2)], map[new(x + 1, y + 1)], map[new(x + 2, y)])
                    is
                        ('M', 'A', 'S')
                        or
                        ('S', 'A', 'M')
            )
            {
                count++;
            }
        }
        return count;
    }
}
