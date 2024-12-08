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
        foreach (var v in min.To(max))
        {
            // Right and Left
            if (
                v.X + 3 <= max.X
                && (
                    map[v],
                    map[v + Vector2.Right],
                    map[v + Vector2.Right * 2],
                    map[v + Vector2.Right * 3]
                )
                    is
                        ('X', 'M', 'A', 'S')
                        or
                        ('S', 'A', 'M', 'X')
            )
            {
                count++;
            }
            // Down and Up
            if (
                v.Y + 3 <= max.Y
                && (
                    map[v],
                    map[v + Vector2.Down],
                    map[v + Vector2.Down * 2],
                    map[v + Vector2.Down * 3]
                )
                    is
                        ('X', 'M', 'A', 'S')
                        or
                        ('S', 'A', 'M', 'X')
            )
            {
                count++;
            }
            // Down right and Up left
            if (
                v.X + 3 <= max.X
                && v.Y + 3 <= max.Y
                && (
                    map[v],
                    map[v + (Vector2.Down + Vector2.Right)],
                    map[v + (Vector2.Down + Vector2.Right) * 2],
                    map[v + (Vector2.Down + Vector2.Right) * 3]
                )
                    is
                        ('X', 'M', 'A', 'S')
                        or
                        ('S', 'A', 'M', 'X')
            )
            {
                count++;
            }
            // Down left and Up right
            if (
                v.X - 3 >= min.X
                && v.Y + 3 <= max.Y
                && (
                    map[v],
                    map[v + (Vector2.Down + Vector2.Left)],
                    map[v + (Vector2.Down + Vector2.Left) * 2],
                    map[v + (Vector2.Down + Vector2.Left) * 3]
                )
                    is
                        ('X', 'M', 'A', 'S')
                        or
                        ('S', 'A', 'M', 'X')
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
        foreach (var v in min.To(max - new Vector2(2, 2)))
        {
            if (
                (map[v], map[v + new Vector2(1, 1)], map[v + new Vector2(2, 2)])
                    is
                        ('M', 'A', 'S')
                        or
                        ('S', 'A', 'M')
                && (
                    map[v + new Vector2(0, 2)],
                    map[v + new Vector2(1, 1)],
                    map[v + new Vector2(2, 0)]
                )
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
